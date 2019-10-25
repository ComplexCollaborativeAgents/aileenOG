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
        self.object_rules = ["[ObjectName]",
                             "[ObjectName] [Relation]",
                             "[Property] [ObjectName]",
                             "[Property] [Property] [ObjectName]",
                             "[Property] [ObjectName] [Relation]",
                             "[ObjectName] [Relation] [Relation]",
                             "[Property] [Property] [Property] [ObjectName]",
                             "[Property] [Property] [ObjectName] [Relation]",
                             "[Property] [ObjectName] [Relation] [Relation]"]
        self.property_names = ["the",
                               "left", "middle", "right",
                               "blue", "red", "yellow"]
        self.relation_rules = ["between [Object] and [Object]",
                               "left of [Object]",
                               "on [Object]",
                               "right of [Object]"]

    def compile_grammar(self):
        replacements = []
        self.append_replacement("[Action]", self.action_rules, replacements)
        self.append_replacement("[Object]", self.object_rules, replacements)
        self.append_replacement("[ObjectName]", self.object_names, replacements)
        self.append_replacement("[Property]", self.property_names, replacements)
        self.append_replacement("[Relation]", self.relation_rules, replacements)
        # self.append_replacement("[Object]", ["[ObjectName]"], replacements)
        # self.append_replacement("[ObjectName]", ["box"], replacements)
        root = self.string_map(["[Object]"])
        print "ROOT"
        print root
        for replacement in replacements:
            print replacement[0]
            print replacement[1]
        return pynini.pdt_replace(root, replacements)
    
    def append_replacement(self, category, rules, replacements):
        if rules != None:
            replacements.append([self.rename_categories(category), self.string_map(rules)])
    
    def string_map(self, strings):
        new_strings = []
        for string in strings:
            new_strings.append(self.rename_categories(string))
        logging.info("[aileen_grammar] :: string_map: {} => {}".format(strings, new_strings))
        return pynini.string_map(new_strings)
    
    def rename_categories(self, string):
        """Rename categories as single characters"""
        # I couldn't get SymbolTable to work.
        string = string.replace("[Action]", "A")
        string = string.replace("[Fragment]", "F")
        string = string.replace("[ObjectName]", "N")
        string = string.replace("[Object]", "O")
        string = string.replace("[Property]", "P")
        string = string.replace("[Relation]", "R")
        return string
    
    def parse(self, sentence):
        if (self.object_names == None):
            self.use_default_rules()
        if (self._transducer == None):
            self._transducer = self.compile_grammar()
            print self._transducer[0]
        output = pynini.pdt_compose(self._transducer[0], sentence, self._transducer[1],
                                    compose_filter="expand_paren").optimize()
        # print output
        iterator = pynini.StringPathIterator(output).ostrings()
        outputs = []
        for string in iterator:
            outputs.append(string)
        return outputs


if __name__ == '__main__':
    grammar = AileenGrammar()
    grammar.use_default_rules()
    outputs = grammar.parse("blue box on box on box")
    # for data in grammar._transducer[0].input_symbols():
        # logging.info("[aileen_grammar] :: transducer symbol: {}".format(data))
    for pair in grammar._transducer[1]:
        logging.info("[aileen_grammar] :: transducer pair: {}".format(pair))
    logging.info("[aileen_grammar] :: test: {}".format(outputs))