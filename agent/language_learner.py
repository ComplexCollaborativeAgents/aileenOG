from log_config import logging
import pynini

class LanguageLearner:

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


if __name__ == '__main__':
    learner = LanguageLearner()
    transducer = learner.construct_cheese_test();
    output_string = learner.apply_transducer(transducer, "Do you have Camembert or Edam?")
    logging.info("[language_learner] :: test: {}".format(output_string))