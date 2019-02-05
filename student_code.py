import read, copy
from util import *
from logical_classes import *

verbose = 0

class KnowledgeBase(object):
    def __init__(self, facts=[], rules=[]):
        self.facts = facts
        self.rules = rules
        self.ie = InferenceEngine()

    def __repr__(self):
        return 'KnowledgeBase({!r}, {!r})'.format(self.facts, self.rules)

    def __str__(self):
        string = "Knowledge Base: \n"
        string += "\n".join((str(fact) for fact in self.facts)) + "\n"
        string += "\n".join((str(rule) for rule in self.rules))
        return string

    def _get_fact(self, fact):
        """INTERNAL USE ONLY
        Get the fact in the KB that is the same as the fact argument

        Args:
            fact (Fact): Fact we're searching for

        Returns:
            Fact: matching fact
        """
        for kbfact in self.facts:
            if fact == kbfact:
                return kbfact

    def _get_rule(self, rule):
        """INTERNAL USE ONLY
        Get the rule in the KB that is the same as the rule argument

        Args:
            rule (Rule): Rule we're searching for

        Returns:
            Rule: matching rule
        """
        for kbrule in self.rules:
            if rule == kbrule:
                return kbrule

    def kb_add(self, fact_rule):
        """Add a fact or rule to the KB
        Args:
            fact_rule (Fact|Rule) - the fact or rule to be added
        Returns:
            None
        """
        printv("Adding {!r}", 1, verbose, [fact_rule])
        if isinstance(fact_rule, Fact):
            if fact_rule not in self.facts:
                self.facts.append(fact_rule)
                for rule in self.rules:
                    self.ie.fc_infer(fact_rule, rule, self)
            else:
                if fact_rule.supported_by:
                    ind = self.facts.index(fact_rule)
                    for f in fact_rule.supported_by:
                        self.facts[ind].supported_by.append(f)
                else:
                    ind = self.facts.index(fact_rule)
                    self.facts[ind].asserted = True
        elif isinstance(fact_rule, Rule):
            if fact_rule not in self.rules:
                self.rules.append(fact_rule)
                for fact in self.facts:
                    self.ie.fc_infer(fact, fact_rule, self)
            else:
                if fact_rule.supported_by:
                    ind = self.rules.index(fact_rule)
                    for f in fact_rule.supported_by:
                        self.rules[ind].supported_by.append(f)
                else:
                    ind = self.rules.index(fact_rule)
                    self.rules[ind].asserted = True

    def kb_assert(self, fact_rule):
        """Assert a fact or rule into the KB

        Args:
            fact_rule (Fact or Rule): Fact or Rule we're asserting
        """
        printv("Asserting {!r}", 0, verbose, [fact_rule])
        self.kb_add(fact_rule)

    def kb_ask(self, fact):
        """Ask if a fact is in the KB

        Args:
            fact (Fact) - Statement to be asked (will be converted into a Fact)

        Returns:
            listof Bindings|False - list of Bindings if result found, False otherwise
        """
        print("Asking {!r}".format(fact))
        if factq(fact):
            f = Fact(fact.statement)
            bindings_lst = ListOfBindings()
            # ask matched facts
            for fact in self.facts:
                binding = match(f.statement, fact.statement)
                if binding:
                    bindings_lst.add_bindings(binding, [fact])

            return bindings_lst if bindings_lst.list_of_bindings else []

        else:
            print("Invalid ask:", fact.statement)
            return []

    def kb_retract(self, fact_or_rule):
        """Retract a fact from the KB

        Args:
            fact (Fact) - Fact to be retracted

        Returns:
            None
        """
        printv("Retracting {!r}", 0, verbose, [fact_or_rule])
        ####################################################
        # Student code goes here

        """if isinstance(fact_or_rule, Fact): ## Given a fact
            self.facts.remove(fact_or_rule) ## Remove The Given Fact
            for f_o_r in fact_or_rule.supports_facts:
                f_o_r.supported_by.remove(fact_or_rule)
                if f_o_r.supported_by.amount() == 0:
                    self.kb_retract(f_o_r) ## recursively remove facts

            for r in fact_or_rule.supports_facts:
                r.supported_by.remove(fact_or_rule)
                if r.supported_by.amount() == 0:
                    self.kb_retract(r) ## recursively remove facts

        elif isinstance(fact_or_rule, Rule):
            if not fact_or_rule.asserted:
                self.rules.remove(fact_or_rule)

                for f_ in fact_or_rule.supports_facts:
                    f_.supported_by.remove(fact_or_rule)
                    if f_.supported_by.amount == 0:
                        self.kb_retract(f_)

                for r_ in fact_or_rule.supports_rules:
                    r_.supported_by.remove(fact_or_rule)
                    if r_.supported_by.amount() == 0:
                        self.kb_retract(r_) """
        if(isinstance(fact_or_rule, Fact) and self.facts.count(self._get_fact(fact_or_rule))): ##Never retract rules, retract means make it not asserted
            fact_or_rule.asserted = False
            if not self._get_fact(fact_or_rule.supported_by):
                self.kb_remove(self._get_fact(fact_or_rule))

    def kb_remove(self, fr):
        isFact = isinstance(fr, Fact)

        if(isFact):
            for f in self._get_fact(fr).supports_facts:
                for fs in self._get_fact(f).supported_by:
                    if fs[0].statement == fr.statement:
                        self._get_fact(f).supported_by.remove(fs)

                if not self._get_fact(f).supported_by: #Indent
                    self.kb_remove(self._get_fact(f))

            for r in self._get_fact(fr).supports_rules:
                for rs in self._get_rule(r).supported_by:
                    if rs[0] == fr.statement:
                        self._get_rule(r).supported_by.remove(rs)

                if not self._get_rule(r).supported_by and not self._get_rule(r).asserted: #indent
                    self.kb_remove(self._get_rule(r))

        elif not fr.asserted:
            for f in self._get_rule(fr).supports_facts:
                for fs in self._get_fact(f).supported_by:
                    if fs[1] == fr:
                        self._get_fact(f).supported_by.remove(fs)

                if not self._get_fact(f).supported_by: #indent
                    self.kb_remove(self._get_fact(f))

            for r in self._get_rule(fr).supports_rules:
                for rs in self._get_rule(r).supported_by:
                    if rs[1] == fr:
                        self._get_rule(r).supported_by.remove(rs)

                if not self._get_rule(r).supported_by and not self._get_rule(r).asserted:
                    self.kb_remove(self._get_rule(r))



        if(isFact):
            if(not self._get_fact(fr).supported_by):
                self.facts.remove(self._get_fact(fr))
        else:
            if(not self._get_rule(fr).supported_by and not self._get_rule(fr).asserted): #check for assertion
                self.rules.remove(self._get_rule(fr))



        

class InferenceEngine(object):
    def fc_infer(self, fact, rule, kb):
        """Forward-chaining to infer new facts and rules

        Args:
            fact (Fact) - A fact from the KnowledgeBase
            rule (Rule) - A rule from the KnowledgeBase
            kb (KnowledgeBase) - A KnowledgeBase

        Returns:
            Nothing            
        """
        printv('Attempting to infer from {!r} and {!r} => {!r}', 1, verbose,
            [fact.statement, rule.lhs, rule.rhs])
        ####################################################
        # Student code goes here
        """for rul in kb.rules:
            if match(fact.statement, rul.lhs[0]): # we want the first element on the left hand side
                #We want to infer the rule first
                kb.rules.append(rul.lhs[1]) # Infer the rest of the rule
                if match(fact.statement, rul.lhs[1]):
                    kb.facts.append(rul.rhs)
                    self.fc_infer(rul.rhs, rul, kb)

        for fac in kb.facts:
            if match(fact.statement, rul.lhs[0]):
                binding = instantiate(fact.statement, rul.lhs[0]) """

        if rule.lhs:
            bindings = match(fact.statement, rule.lhs[0])
            if bindings:
                if len(rule.lhs) == 1:
                    info = instantiate(rule.rhs, bindings)
                    n_fact = Fact(info, [[fact, rule]])
                    n_fact.asserted = False
                    kb.kb_assert(n_fact)
                    rule.supports_facts.append(kb._get_fact(n_fact))
                    fact.supports_facts.append(kb._get_fact(n_fact))
                else:
                    r_or_rule = rule.lhs[1:]
                    lef = []
                    for p in r_or_rule:
                        info = instantiate(p, bindings)
                        lef.append(info)
                    new_rhs = instantiate(rule.rhs, bindings)
                    n_rule = Rule([lef, new_rhs], [[fact, rule]])
                    n_rule.asserted = False
                    kb.kb_assert(n_rule)
                    rule.supports_rules.append(kb._get_rule(n_rule))
                    fact.supports_rules.append(kb._get_rule(n_rule))
                    n_rule.asserted = False #This one works



















