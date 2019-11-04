from log_config import logging
import aileen_grammar

class LanguageLearner:
    """The LanguageLearner tries to learn the grammar of the instructor's natural language input.
    It works by abduction: it tries to find the smallest number of rules that need to be added
    to make the input parsable.  The result is an AileenGrammar.
    """
    
    _grammar = aileen_grammar.AileenGrammar()
    
    def set_grammar(self, grammar):
        """Set the initial grammar used by the language learner.
        If this function is not called then the LanguageLearner starts with an empty grammar."""
        self._grammar = grammar
    
    def get_grammar(self):
        """Return the grammar that has been learned."""
        return self._grammar
    
    def parse_description(self, sentence, scene=None):
        """Parse a natural language description of a scene.
        Learns grammar rules as a side effect.
        The scene, if given, should implement ground_object_description(description).
        ground_object_description should take a typed list (e.g. ['obj' ['prop' 'blue'] 'box'])
        and should return a list of objects matching the description."""
        # Parse the sentence with the current grammar and disambiguate it against the scene.
        parses = self._grammar.parse(sentence)
        parses = self._disambiguate(parses, scene)
        if len(parses) > 1:
            logging.warn("[language_learner] :: parse_description: {} cannot be disambiguated against {}".format(sentence, scene))
            parses = parses[0]
        # Guess the missing rules.
        labeled_rules = self._guess_description_rules(parses[0])
        # Add the rules to the grammar.
        for rule in labeled_rules:
            old_rules = self._get_grammar_rules(rule[0])
            old_rules.append(rule[1])
            self._grammar.reset_parser()
            logging.info("[language_learner] :: parse_description: adding '{}' to {}".format(rule[1], rule[0]))
        # Parse the sentence again with the new grammar.
        return self._grammar.parse(sentence)

    def _disambiguate(self, parses, scene):
        """Disambiguate the parses against the scene."""
        return parses

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
            rule = ""
            for item in parse[2:]:
                if item[0] == 'obj':
                    rule = self._add_token(rule, "[obj]")
                elif item[0] == 'prop':
                    rule = self._add_token(rule, "[prop]")
                elif item[0] == 'rel':
                    rule = self._add_token(rule, "[rel]")
                else:
                    rule = self._add_token(rule, item)
            self._append_new_rule(labeled_rules, ['rel', rule])
            # Extract object rule for initial object plus a relation.
            rule = self._convert_parse_to_rule(parse[1]) + " [rel]"
            self._append_new_rule(labeled_rules, ['obj', rule])
        else:
            logging.warn("[language_learner] :: guess_description_rules: cannot guess {}".format(parse))
        return labeled_rules

    def _convert_parse_to_rule(self, parse):
        """Convert the given parse into a rule."""
        rule = ""
        parse_type = parse[0]
        parse = parse[1:]
        obj_name_added = False
        for item in parse:
            if type(item) == list:
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

    def _get_grammar_rules(self, rule_type):
        if rule_type == 'action':
            return self._grammar.action_rules
        elif rule_type == 'obj':
            return self._grammar.object_rules
        elif rule_type == 'obj_name':
            return self._grammar.object_names
        elif rule_type == 'prop':
            return self._grammar.property_names
        elif rule_type == 'rel':
            return self._grammar.relation_rules
        else:
            raise Exception("unknown rule type: {}".format(type))

if __name__ == '__main__':
    learner = LanguageLearner()
