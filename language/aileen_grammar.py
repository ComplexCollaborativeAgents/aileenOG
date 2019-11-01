from log_config import logging
import pynini

class AileenGrammar:
    """An AileenGrammar contains grammar rules for aileen's syntax.
    Grammar rules are strings that use square brackets to mark categories.
    For example, 'between [obj] and [obj]' is a grammar rule for the 'between' relation.
    Here are the different categories of rules:
        action_rules give the syntax of commands.  They produce an 'action' type.
        object_names give the classes of objects.
        object_rules give the syntax of objects.  They produce an 'obj' type.
        property_names give the classes properties.  They produce a 'prop' type.
        relation_rules give the syntax of relations.  They produce a 'rel' type."""
    action_rules = None
    object_names = None
    object_rules = None
    property_names = None
    relation_rules = None

    _parser = None
    _fragment_parser = None

    def reset_rules(self):
        """Reset the rules to None."""
        self.action_rules = None
        self.object_names = None
        self.object_rules = None
        self.property_names = None
        self.relation_rules = None
        self.reset_parser()

    def reset_parser(self):
        """Reset the parser so that it can be rebuilt from the latest grammar rules."""
        self._parser = None
        self._fragment_parser = None

    def use_default_rules(self):
        """Create a grammar with default rules."""
        self.object_names = ["box", "block", "cylinder", "pyramid", "sphere"]
        self.object_rules = ["[obj_name]",
                             "[obj_name] [rel]",
                             "[prop] [obj_name]",
                             "[prop] [prop] [obj_name]",
                             "[prop] [obj_name] [rel]",
                             "[obj_name] [rel] [rel]",
                             "[prop] [prop] [prop] [obj_name]",
                             "[prop] [prop] [obj_name] [rel]",
                             "[prop] [obj_name] [rel] [rel]"]
        self.property_names = ["the",
                               "left", "middle", "right",
                               "blue", "red", "yellow"]
        self.relation_rules = ["between [obj] and [obj]",
                               "left of [obj]",
                               "on [obj]",
                               "right of [obj]"]

    def _compile_rules(self):
        """Compile the grammar rules into a transducer."""
        replacements = []
        # STANDARD PARSER
        self._append_replacement("[action]", self.action_rules, replacements)
        self._append_replacement("[obj]", self.object_rules, replacements)
        self._append_replacement("[obj_name]", self.object_names, replacements)
        self._append_replacement("[prop]", self.property_names, replacements)
        self._append_replacement("[rel]", self.relation_rules, replacements)
        root = self._string_map(["[action]", "[obj]"])
        self._parser = pynini.pdt_replace(root.optimize(), replacements)
        # FRAGMENT PARSER
        word = self._string_map(["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                                 "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"])
        # word = char+
        word = word.closure(1).optimize()
        replacements.append([self._rename_categories("[word]"), word])
        # fragments = [fragment]+
        self._append_replacement("[fragment]", ["[action]", "[obj]", "[prop]", "[rel]", "[word]"], replacements)
        fragments = self._convert_rule_to_fst("[fragment]").optimize()
        fragments.concat(self._convert_rule_to_fst(" [fragment]").closure(0)).optimize()
        replacements.append([self._rename_categories("[fragments]"), fragments])
        root = self._string_map(["[fragments]"])
        self._fragment_parser = pynini.pdt_replace(root.optimize(), replacements)
    
    def _append_replacement(self, category, rules, replacements):
        if rules != None:
            replacements.append([self._rename_categories(category), self._string_map(rules)])
    
    def _string_map(self, rules):
        """Create an FST that represents the union of the given rules."""
        fst = None
        for rule in rules:
            rule_fst = self._convert_rule_to_fst(rule)
            if fst is None:
                fst = rule_fst
            else:
                fst = pynini.union(fst, rule_fst)
        return fst.optimize()
    
    def _convert_rule_to_fst(self, rule):
        """Convert the given rule into an FST."""
        if (rule.lower() != rule):
            # We are using capital letters to push and pop transducers.
            raise Exception("rule cannot contain capital letters: {}".format(rule))
        # Replace categories like [obj] with a character label.
        # Some categories also have start and end tags inserted around the character label.
        fst = pynini.acceptor("")
        # Look for the next category.
        last_index = 0;
        index = rule.find("[", last_index)
        tag_cats = ["[action]", "[fragments]", "[obj]", "[prop]", "[rel]"]
        while (index >= 0):
            # Append anything between the last category and this category.
            if (last_index < index):
                fst.concat(pynini.acceptor(rule[last_index:index]))
            # Find the end of the category.
            index2 = rule.find("]", index)
            if (index2 < 0):
                raise Exception("missing ] in {}".format(rule))
            cat = rule[index:index2+1]
            # Add a start tag.
            if (cat in tag_cats):
                fst.concat(pynini.transducer("", "<" + cat[1:-1] + ">"))
            # Convert the category to a label.
            fst.concat(pynini.acceptor(self._rename_categories(cat)))
            # Add an end tag.
            if (cat in tag_cats):
                fst.concat(pynini.transducer("", "</" + cat[1:-1] + ">"))
            # Look for the next category.
            last_index = index2 + 1
            index = rule.find("[", last_index)
        # Append anything that remains.
        if (last_index < len(rule)):
            fst.concat(pynini.acceptor(rule[last_index:]))
        return fst.optimize()

    def _rename_categories(self, string):
        """Rename categories as single characters"""
        # JTM: I couldn't get SymbolTable to work, so I use capital letters to represent categories.
        # TODO: Figure out how to use SymbolTable to represent pushdown arcs.
        string = string.replace("[action]", "A")
        string = string.replace("[fragment]", "F")
        string = string.replace("[fragments]", "G")
        string = string.replace("[obj_name]", "N")
        string = string.replace("[obj]", "O")
        string = string.replace("[prop]", "P")
        string = string.replace("[rel]", "R")
        string = string.replace("[word]", "W")
        return string
    
    def parse(self, sentence):
        """Parse the given sentence using a parser compiled from the current grammar.
        If there are no grammar rules, uses the default rules.
        Returns a typed list (e.g. "blue box" => [obj [prop blue] box])."""
        if (self.object_names == None):
            self.use_default_rules()
        if (self._parser == None):
            self._compile_rules()
        # Parse using pdt_compose.
        fst = pynini.pdt_compose(sentence, self._parser[0], self._parser[1],
                                compose_filter="expand", left_pdt=False).optimize()
        if fst.num_states() == 0:
            # Try parsing with the fragment parser.
            fst = pynini.pdt_compose(sentence, self._fragment_parser[0], self._fragment_parser[1],
                                     compose_filter="expand", left_pdt=False).optimize()
        # get output
        # TODO: Figure out why we can't used weighted FSTs to find the parses with the fewest fragments.
        outputs = []
        for string in pynini.StringPathIterator(fst).ostrings():
            outputs.append(self._convert_markup_to_list(string))
        outputs = self._filter_fragments(outputs)
        logging.info("[aileen_grammar] :: parse({}) = {}".format(sentence, outputs))
        return outputs

    def _convert_markup_to_list(self, string):
        """Convert strings like "<obj><prop>blue</prop> box</obj>" to lists like [obj [prop blue] box]."""
        result = []
        stack = []
        stack.append(result)
        token = ""
        index = 0
        while (index < len(string)):
            if (string[index] == "<"):
                # We found a tag.
                # First, deal with the token, if any.
                if (token != ""):
                    stack[-1].append(token)
                token = ""
                # Then find the rest of the tag.
                index2 = string.find(">", index)
                cat = string[index+1:index2]
                if (cat[0] == "/"):
                    # We found an end tag.  Pop the stack.
                    stack.pop()
                else:
                    # We found a start tag.  Start a new list.
                    new_list = [cat]
                    stack[-1].append(new_list)
                    stack.append(new_list)
                index = index2
            elif (string[index] == " "):
                # A space marks the end of a token.
                if (token != ""):
                    stack[-1].append(token)
                token = ""
            else:
                # Add the character to the current token.
                token += string[index]
            index = index + 1
        assert (len(token) == 0)
        assert (len(stack) == 1)
        return result[0]

    def _filter_fragments(self, parses):
        """Filter high scoring fragments from parses"""
        # Find the minimum score for the parses.
        min_score = -1
        has_fragments = False
        for parse in parses:
            score = self._fragment_score(parse)
            if min_score == -1 or score < min_score:
                min_score = score
            if parse[0] == 'fragments':
                has_fragments = True
        if has_fragments is False:
            return parses
        # Collect parses that have min_score.
        new_parses = []
        for parse in parses:
            score = self._fragment_score(parse)
            if score == min_score:
                new_parses.append(parse)
        return new_parses

    def _fragment_score(self, parse):
        if parse[0] != 'fragments':
            return 0
        score = 0
        for item in parse:
            if type(item) is list:
                score += 1
            else:
                score += 2
        return score


if __name__ == '__main__':
    grammar = AileenGrammar()
    outputs = grammar.parse("blue blue")
    for output in outputs:
        logging.info("[aileen_grammar] :: test: {}".format(output))
