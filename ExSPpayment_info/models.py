from __future__ import division

import random

from otree.constants import BaseConstants
from otree.models import BaseSubsession, BaseGroup, BasePlayer, BaseLink

from otree.db import models
from otree import widgets
from otree.common import Currency as c, currency_range

doc = """
This application provides a webpage instructing participants how to get paid.
Examples are given for the lab and Amazon Mechanical Turk (AMT).
"""


class Constants(BaseConstants):
    name_in_url = 'ExSPpayment_info'
    players_per_group = None
    num_rounds = 1

class Subsession(BaseSubsession):
    pass

class Group(BaseGroup):
    pass


class Link(BaseLink):
    pass


class Player(BasePlayer):
    finalpay = models.FloatField()
    ffinalpay = models.FloatField()
    '''
    def pay1(self):
        fin1 = [p.fi for p in self.participant.vars.get("pay1")()]
        fina1 = sum(p.f for p in fin1)
        return fina1
    '''
    def pa1(self):
        fin1 = self.participant.vars.get("pay1")
        fina1 = float(fin1)
        return fina1

    def pa2(self):
        fin2 = self.participant.vars.get("pay2")
        fina2 = float(fin2)
        return fina2

    def pa3(self):
        fin2 = self.participant.vars.get("pay3")
        fina2 = float(fin2)
        return fina2

    def fipay(self):
        self.finalpay = self.pa1() + self.pa2() + self.pa3()
        final_payoff = self.pa1() + self.pa2() + self.pa3()
        return final_payoff

    def finapay(self):
        pon = self.fipay()/50
        self.ffinalpay =  pon + 5
        ffinal_payoff = pon + 5
        return ffinal_payoff
    '''
    def fipay(self):
        self.finalpay = sum([p.final for p in self.participant.vars.get("pay1")]) + sum([p.final for p in self.participant.vars.get("pay2")])
        final_payoff =  sum([p.final for p in self.participant.vars.get("pay1")]) + sum([p.final for p in self.participant.vars.get("pay2")])
        return (final_payoff)
    '''

