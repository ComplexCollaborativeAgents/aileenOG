from log_config import logging
import aileen_grammar

class LanguageLearner:
    
    _parser = aileen_grammar.AileenGrammar()
    _parser.reset_rules()
    
    def set_parser(self, parser):
        """Set the parser used by the language learner."""
        self._parser = parser
    
    def get_parser(self):
        """Return the parser that has been learned."""
        return self._parser
    
    def parse_description(self, sentence, scene=None):
        """Parse a natural language description of a scene.
        The scene should implement ground_object_description(list_description),
        which returns a list of objects matching the description."""
        # Parse the sentence with the current grammar and disambiguate it against the scene.
        parses = self._parser.parse(sentence)
        parses = self._disambiguate(parses, scene)
        if len(parses) > 1:
            logging.warn("[language_learner] :: parse: {} cannot be disambiguated against {}".format(sentence, scene))
            parses = parses[0]
        # Guess the missing rules.
        rules = self._guess_description_rules(parses[0])
        # Add the missing rules to the parser.
        for rule in rules:
            if rule[0] == 'obj':
                self._parser.object_rules.append(rule[1])
            if rule[1] == 'obj_name':
                self._parser.object_names.append(rule[1])
            if rule[1] == 'prop':
                self._parser.property_names.append(rule[1])
            self._parser.reset_parser()
        # Parse the sentence again with the new grammar.
        return self._parser.parse(sentence)

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
        logging.info("[language_learner] :: guess_description_rules: {} => {}".format(parse, labeled_rules))
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
            if labeled_rule[1] not in self._parser.object_rules:
                labeled_rules.append(labeled_rule)
        elif labeled_rule[0] == 'obj_name':
            if labeled_rule[1] not in self._parser.object_names:
                labeled_rules.append(labeled_rule)
        elif labeled_rule[0] == 'prop':
            if labeled_rule[1] not in self._parser.property_names:
                labeled_rules.append(labeled_rule)
        else:
            raise Exception("unknown rule type: {}".format(labeled_rule[0]))


if __name__ == '__main__':
    learner = LanguageLearner()
