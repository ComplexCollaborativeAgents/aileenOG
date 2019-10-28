from log_config import logging
import pynini

class AileenGrammar:
    """A grammar for aileen NL as a set of rules"""
    action_rules = None
    object_names = None
    object_rules = None
    property_names = None
    relation_rules = None
    
    _transducer = None

    def use_default_rules(self):
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

    def compile_grammar(self):
        replacements = []
        self.append_replacement("[action]", self.action_rules, replacements)
        self.append_replacement("[obj]", self.object_rules, replacements)
        self.append_replacement("[obj_name]", self.object_names, replacements)
        self.append_replacement("[prop]", self.property_names, replacements)
        self.append_replacement("[rel]", self.relation_rules, replacements)
        root = self.string_map(["[obj]"]).optimize()
        return pynini.pdt_replace(root, replacements)
    
    def append_replacement(self, category, rules, replacements):
        if rules != None:
            replacements.append([self.rename_categories(category), self.string_map(rules)])
    
    def string_map(self, strings):
        fst = None
        for string in strings:
            rule_fst = self.convert_rule_to_fst(string)
            if fst is None:
                fst = rule_fst
            else:
                fst = pynini.union(fst, rule_fst)
        return fst.optimize()
    
    def convert_rule_to_fst(self, rule):
        """Convert the given rule into an FST."""
        if (rule.lower() != rule):
            # We are using capital letters to push and pop transducers.
            raise Exception("rule cannot contain capital letters: {}".format(rule))
        last_index = 0;
        index = rule.find("[", last_index)
        fst = pynini.acceptor("")
        while (index >= 0):
            if (last_index < index):
                fst.concat(pynini.acceptor(rule[last_index:index]))
            index2 = rule.find("]", index)
            if (index2 < 0):
                raise Exception("missing ] in {}".format(rule))
            cat = rule[index:index2+1]
            if (cat in ["[action]", "[obj]", "[prop]", "[rel]"]):
                fst.concat(pynini.transducer("", "<" + cat[1:-1] + ">"))
            fst.concat(pynini.acceptor(self.rename_categories(cat)))
            if (cat in ["[action]", "[obj]", "[prop]", "[rel]"]):
                fst.concat(pynini.transducer("", "</" + cat[1:-1] + ">"))
            last_index = index2 + 1
            index = rule.find("[", last_index)
        if (last_index < len(rule)):
            fst.concat(pynini.acceptor(rule[last_index:]))
        return fst.optimize()

    def rename_categories(self, string):
        """Rename categories as single characters"""
        # I couldn't get SymbolTable to work.
        string = string.replace("[action]", "A")
        string = string.replace("[fragments]", "F")
        string = string.replace("[obj_name]", "N")
        string = string.replace("[obj]", "O")
        string = string.replace("[prop]", "P")
        string = string.replace("[rel]", "R")
        return string
    
    def parse(self, sentence):
        if (self.object_names == None):
            self.use_default_rules()
        if (self._transducer == None):
            self._transducer = self.compile_grammar()
        output = pynini.pdt_compose(sentence, self._transducer[0], self._transducer[1],
                                    compose_filter="expand", left_pdt=False).optimize()
        # print output
        iterator = pynini.StringPathIterator(output).ostrings()
        outputs = []
        for string in iterator:
            outputs.append(string)
        return outputs


if __name__ == '__main__':
    grammar = AileenGrammar()
    grammar.use_default_rules()
    outputs = grammar.parse("blue box on box")
    # outputs = grammar.parse("blue box on box on box")
    logging.info("[aileen_grammar] :: test: {}".format(outputs))
