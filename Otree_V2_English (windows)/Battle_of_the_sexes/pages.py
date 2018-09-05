from . import models
from ._builtin import Page, WaitPage
from otree.api import Currency as c, currency_range
from .models import Constants


class Introduction(Page):
    def is_displayed(self):
        return self.subsession.round_number == 1


class Page1(Page):
    def is_displayed(self):
        return self.subsession.round_number == 1

class Questions(Page):
    form_model = 'player'
    form_fields = ['question1', 'question2', 'question3', 'question4', 'question5', 'question6', 'question7']

    def is_displayed(self):
        return self.subsession.round_number == 1

class Check(Page):
    def is_displayed(self):
        return self.subsession.round_number == 1


class PractiseInstructions(Page):
    def is_displayed(self):
        return self.subsession.round_number == 1


class Decide(Page):
    form_model = 'player'
    form_fields = ['Blue']


class ResultsWaitPage(WaitPage):
    def after_all_players_arrive(self):
        for p in self.group.get_players():
            p.set_payoff()

    body_text = "Waiting for the other participants."


class Results(Page):
    pass


page_sequence = [Decide,
                 ResultsWaitPage,
                 Results]
'''
page_sequence = [Page1,
                 Introduction,
                 Questions,
                 Check,
                 PractiseInstructions,
                 Decide,
                 ResultsWaitPage,
                 Results]
'''