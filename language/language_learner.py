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
            logging.warn("[language_learner] :: parse: {} cannot be disambiguated against {}".format(sentence, scene))
            parses = parses[0]
        # Guess the missing rules.
        rules = self._guess_description_rules(parses[0])
        # Add the missing rules to the grammar.
        for rule in rules:
            logging.info("[language_learner] :: parse_description adding {} to {}".format(rule[1], rule[0]))
            if rule[0] == 'obj':
                self._grammar.object_rules.append(rule[1])
            if rule[0] == 'obj_name':
                self._grammar.object_names.append(rule[1])
            if rule[0] == 'prop':
                self._grammar.property_names.append(rule[1])
            self._grammar.reset_parser()
        # Parse the sentence again with the new grammar.
        return self._grammar.parse(sentence)

    def _disambiguate(self, parses, scene):
        """Disambiguate the parses against the scene."""
        return parses

    def _guess_description_rules(self, parse):
        """Guess the missing description rules."""
        if parse[0] == 'obj':
            # No missing rules.
            return []
        labeled_rules = []
        # Count the objects in the parse.        
        object_count = 0
        for item in parse:
            if item[0] == 'obj':
                object_count += 1
        if object_count == 0:
            # The whole parse represents an object name.
            self._append_new_rule(labeled_rules, ['obj_name', self._convert_parse_to_rule(parse)])
            self._append_new_rule(labeled_rules, ['obj', "[obj_name]"])
        if object_count == 1:
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
                logging.warn("[language_learner] :: guess_description_rules: {} has left and right properties".format(parse))
            if left != "":
                self._append_new_rule(labeled_rules, ['prop', left])
                rule = "[prop] " + self._convert_parse_to_rule(obj)
                self._append_new_rule(labeled_rules, ['obj', rule])
            if right != "":
                self._append_new_rule(labeled_rules, ['prop', right])
                rule = self._convert_parse_to_rule(obj) + " [prop]"
                self._append_new_rule(labeled_rules, ['obj', rule])
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
        """Append labeled_rule to labeled_rules if it is new."""
        if labeled_rule[0] == 'obj':
            if labeled_rule[1] not in self._grammar.object_rules:
                labeled_rules.append(labeled_rule)
        elif labeled_rule[0] == 'obj_name':
            if labeled_rule[1] not in self._grammar.object_names:
                labeled_rules.append(labeled_rule)
        elif labeled_rule[0] == 'prop':
            if labeled_rule[1] not in self._grammar.property_names:
                labeled_rules.append(labeled_rule)
        else:
            raise Exception("unknown rule type: {}".format(labeled_rule[0]))


if __name__ == '__main__':
    learner = LanguageLearner()
