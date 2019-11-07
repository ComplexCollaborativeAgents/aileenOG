from log_config import logging
import aileen_grammar

class LanguageLearner:
    """The LanguageLearner tries to learn the grammar of the instructor's natural language input.
    It works by abduction: it tries to find the smallest number of rules that need to be added
    to make the input parsable.  The result is an AileenGrammar.
    """
    def __init__(self, grammar=None):
        if grammar == None:
            grammar = aileen_grammar.AileenGrammar()
        self.grammar = grammar
    
    def parse_description(self, sentence, scene=None):
        """Parse a natural language description of a scene.
        Learns grammar rules as a side effect.
        The scene, if given, should implement ground_object_description(description).
        ground_object_description should take a typed list (e.g. ['obj' ['prop' 'blue'] 'box'])
        and should return a list of objects matching the description."""
        # Parse the sentence with the current grammar and disambiguate it against the scene.
        parses = self.grammar.parse(sentence)
        parses = self._disambiguate(parses, scene)
        if len(parses) > 1:
            logging.warn("[language_learner] :: parse_description: {} cannot be disambiguated against {}".format(sentence, scene))
            parses = [parses[0]]
        # Guess the missing rules.
        labeled_rules = self._guess_description_rules(parses[0])
        self._add_rules_to_grammar(labeled_rules)
        # Parse the sentence again with the new grammar.
        return self.grammar.parse(sentence)

    def parse_action(self, sentence, scenes=[]):
        """Parse a natural language representation of an action.
        Learns grammar rules as a side effect.
        The scenes, if given, should correspond to the set of action states.
        The scenes should implement ground_object_description(description).
        ground_object_description should take a typed list (e.g. ['obj' ['prop' 'blue'] 'box'])
        and should return a list of objects matching the description."""
        # Parse the sentence with the current grammar and disambiguate it against the scenes.
        parses = self.grammar.parse(sentence)
        parses = self._disambiguate(parses, scenes)
        if len(parses) > 1:
            logging.warn("[language_learner] :: parse_action: {} cannot be disambiguated against {}".format(sentence, scenes))
            parses = [parses[0]]
        # Guess the missing rules
        labeled_rules = self._guess_action_rules(parses[0])
        self._add_rules_to_grammar(labeled_rules)
        # Parse the sentence again with the new grammar.
        return self.grammar.parse(sentence)

    def _disambiguate(self, parses, scenes):
        """Disambiguate the parses using the scenes."""
        new_parses = []
        for parse in parses:
            new_parse = self._ground_parse_using_scene_info(parse, scenes)
            new_parses.append(new_parse)
        if len(new_parses) == 1:
            return new_parses
        # Find the simplest parses.
        min_size = 10000
        for parse in new_parses:
            if len(parse) < min_size:
                min_size = len(parse)
        min_parses = []
        for parse in new_parses:
            if len(parse) == min_size:
                min_parses.append(parse)
        return min_parses

    def _ground_parse_using_scene_info(self, parse, scenes):
        """Ground the items in a fragment parse using the scenes."""
        if scenes == None or len(scenes) == 0:
            # Cannot ground the objects in parse.
            return parse
        if parse[0] != 'fragments':
            return parse
        new_parse = []
        for item in parse:
            if type(item) is list:
                new_items = self._split_object_using_scene_info(item, scenes)
                new_parse.extend(new_items)
            else:
                new_parse.append(item)
        return new_parse

    def _split_object_using_scene_info(self, item, scenes):
        """Split the given item into parts based on the scenes."""
        if type(scenes) is not list:
            scenes = [scenes]
        assert type(item) is list
        if item[0] == 'obj':
            object_ = item
        else:
            # Convert item into an object.
            object_ = ['obj'] + item
        # Check how many scenes object_ appears in.
        appearances = self._get_appearances(object_, scenes)
        # Keep the item together if object_ appears in every scene.
        if len(appearances) == len(scenes):
            return [item]
        # Keep the item together if it appears in the first scene.
        if scenes[0] in appearances:
            return [item]
        # Split the object if it appears in one scene and part of it appears in the others.
        # Example: "move block left of cylinder"
        if len(appearances) > 0 and len(object_) > 2:
            other_scenes = list(set(scenes) - set(appearances))
            if type(object_[-1]) is list:
                # Try removing a trailing item from object_.
                sub_object = object_[:-1]
                scenes2 = self._get_appearances(sub_object, other_scenes)
                if len(scenes2) == len(other_scenes):
                    return [sub_object, object_[-1]]
            if type(object_[1]) is list:
                # Try removing an initial item from object_.
                sub_object = ['obj'] + object_[2:]
                scenes2 = self._get_appearances(sub_object, other_scenes)
                if len(scenes2) == len(other_scenes):
                    return [object_[1], sub_object]
        # Return the item if object_ appears in any scene.
        if len(appearances) > 0:
            return [item]
        # Otherwise, recurse on item's parts.
        new_items = []
        for part in item[1:]:
            if type(part) is list:
                new_parts = self._split_object_using_scene_info(part, scenes)
                new_items.extend(new_parts)
            else:
                new_items.append(part)
        return new_items

    def _get_appearances(self, object_, scenes):
        appearances = []
        for scene in scenes:
            objects = scene.ground_object_description(object_)
            if len(objects) > 0:
                appearances.append(scene)
        return appearances

    def _guess_description_rules(self, parse):
        """Guess the description rules needed to complete a fragment parse."""
        if parse[0] != 'fragments':
            # No missing rules.
            return []
        labeled_rules = []
        # Count the different types in the parse.
        action_count = 0
        object_count = 0
        property_count = 0
        relation_count = 0
        for item in parse:
            if item[0] == 'action':
                action_count += 1
            if item[0] == 'obj':
                object_count += 1
            if item[0] == 'prop':
                property_count += 1
            if item[0] == 'rel':
                relation_count += 1
        total_count = action_count + object_count + property_count + relation_count
        if total_count == 0:
            # The whole parse represents an object name.
            self._append_new_rule(labeled_rules, ['obj_name', self._convert_parse_to_rule(parse)])
            self._append_new_rule(labeled_rules, ['obj', "[obj_name]"])
        elif total_count == 1 and object_count == 1:
            # Look for properties to the left and right of the object.
            left = ""
            right = ""
            obj = None
            for item in parse[1:]:
                if item[0] == 'obj':
                    obj = item
                elif obj is None:
                    left = self._add_token(left, item)
                else:
                    right = self._add_token(right, item)
            if left != "" and right != "":
                logging.warn("[language_learner] :: guess_description_rules: '{}' has left and right properties".format(parse))
            if left != "":
                self._append_new_rule(labeled_rules, ['prop', left])
                rule = "[prop] " + self._convert_parse_to_rule(obj)
                self._append_new_rule(labeled_rules, ['obj', rule])
            if right != "":
                self._append_new_rule(labeled_rules, ['prop', right])
                rule = self._convert_parse_to_rule(obj) + " [prop]"
                self._append_new_rule(labeled_rules, ['obj', rule])
        elif total_count > 1 and parse[1][0] == 'obj':
            # Extract relation rule for portion after initial object.
            rule = self._convert_parse_to_rule(['rel'] + parse[2:])
            self._append_new_rule(labeled_rules, ['rel', rule])
            # Extract object rule for initial object plus a relation.
            rule = self._convert_parse_to_rule(parse[1]) + " [rel]"
            self._append_new_rule(labeled_rules, ['obj', rule])
        else:
            logging.warn("[language_learner] :: guess_description_rules: cannot guess {}".format(parse))
        return labeled_rules

    def _guess_action_rules(self, parse):
        """Guess the action rules needed to complete parse."""
        if parse[0] != 'fragments':
            # No missing rules.
            return []
        rule = self._convert_parse_to_rule(['action'] + parse[1:])
        return [['action', rule]]

    def _convert_parse_to_rule(self, parse):
        """Convert the given parse into a rule."""
        rule = ""
        parse_type = parse[0]
        parse = parse[1:]
        obj_name_added = False
        for item in parse:
            if type(item) is list:
                rule = self._add_token(rule, "[" + item[0] + "]")
            elif parse_type == 'obj':
                # An unlabeled item becomes [obj_name] one time.
                if not obj_name_added:
                    rule = self._add_token(rule, "[obj_name]")
                    obj_name_added = True
            else:
                rule = self._add_token(rule, item)
        return rule              
 
    def _add_token(self, rule, token):
        """Add a token to a rule."""
        if rule == "":
            return token
        return rule + " " + token

    def _append_new_rule(self, labeled_rules, labeled_rule):
        """If labeled_rule is new, append it to labeled_rules and the grammar rules."""
        old_rules = self._get_grammar_rules(labeled_rule[0])
        if labeled_rule[1] not in old_rules:
            labeled_rules.append(labeled_rule)

    def _add_rules_to_grammar(self, labeled_rules):
        """Add the labeled rules to the grammar."""
        for rule in labeled_rules:
            old_rules = self._get_grammar_rules(rule[0])
            old_rules.append(rule[1])
            self.grammar.reset_parser()
            logging.info("[language_learner] :: adding '{}' to {} rule".format(rule[1], rule[0]))

    def _get_grammar_rules(self, rule_type):
        """Get the grammar rules for the given rule_type."""
        if rule_type == 'action':
            return self.grammar.action_rules
        elif rule_type == 'obj':
            return self.grammar.object_rules
        elif rule_type == 'obj_name':
            return self.grammar.object_names
        elif rule_type == 'prop':
            return self.grammar.property_names
        elif rule_type == 'rel':
            return self.grammar.relation_rules
        else:
            raise Exception("unknown rule type: {}".format(rule_type))


if __name__ == '__main__':
    learner = LanguageLearner()
