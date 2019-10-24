from log_config import logging
import pynini

class LanguageLearner:

    def compile_grammar(self, grammar):
        replacements = []
        replacements.append(["ACTION", pynini.string_map(grammar.action_rules)])
        replacements.append(["OBJECT_NAMES", pynini.string_map(grammar.object_names)])
        replacements.append(["OBJECT", pynini.string_map(grammar.object_rules)])
        replacements.append(["PROPERTY", pynini.string_map(grammar.property_names)])
        replacements.append(["RELATION", pynini.string_map(grammar.relation_rules)])
        return pynini.pdt_replace(root, replacements)

    def default_grammar(self):
        grammar = Grammar()
        grammar.object_names = ["box", "block", "cylinder", "pyramid", "sphere"]
        grammar.object_rules = ["OBJECT_NAMES",
                                "OBJECT_NAMES RELATION"
                                "PROPERTY OBJECT_NAMES",
                                "PROPERTY PROPERTY OBJECT_NAMES",
                                "PROPERTY OBJECT_NAMES RELATION",
                                "OBJECT_NAMES RELATION RELATION",
                                "PROPERTY PROPERTY PROPERTY OBJECT_NAMES",
                                "PROPERTY PROPERTY OBJECT_NAMES RELATION",
                                "PROPERTY OBJECT_NAMES RELATION RELATION"]
        grammar.property_names = ["the",
                                  "left", "middle", "right",
                                  "blue", "red", "yellow"]
        grammar.relation_rules = ["between OBJECT and OBJECT",
                                  "left of OBJECT",
                                  "on OBJECT",
                                  "right of OBJECT"]

    def construct_cheese_test(self):
        cheeses = ("Boursin", "Camembert", "Cheddar", "Edam", "Gruyere",
           "Ilchester", "Jarlsberg", "Red Leicester", "Stilton")
        fst_target = pynini.string_map(cheeses)
        ltag = pynini.transducer("", "<cheese>")
        rtag = pynini.transducer("", "</cheese>")
        substitution = ltag + fst_target + rtag
        chars = ([chr(i) for i in xrange(1, 91)] + 
         ["\[", "\\", "\]"] + 
         [chr(i) for i in xrange(94, 256)])
        seprs = [" ", "?", ".", "!", ",", ";", ":"]
        sigma_star = pynini.string_map(chars).closure()
        context = pynini.string_map(seprs)
        rewrite = pynini.cdrewrite(substitution, context, context, sigma_star)
        return rewrite

    def apply_transducer(self, transducer, input_string):
        output = pynini.compose(input_string, transducer).optimize()
        iterator = pynini.StringPathIterator(output).ostrings()
        outputs = []
        for string in iterator:
            outputs.append(string)
        return outputs


class Grammar:
    """A grammar as a set of rules"""
    action_rules = None
    object_names = None
    object_rules = None
    property_names = None
    relation_rules = None

if __name__ == '__main__':
    learner = LanguageLearner()
    transducer = learner.construct_cheese_test();
    output_string = learner.apply_transducer(transducer, "Do you have Camembert or Edam?")
    logging.info("[language_learner] :: test: {}".format(output_string))