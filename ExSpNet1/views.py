from . import models
from ._builtin import Page, WaitPage
from otree.api import Currency as c, currency_range
from .models import Constants
import time


class Introduction(Page):
    def is_displayed(self):
        return self.subsession.round_number == 1


class Page1(Page):
    def is_displayed(self):
        return self.subsession.round_number == 1

class Questions(Page):
    form_model = models.Player
    form_fields = ['question1', 'question2', 'question3', 'question4', 'question5', 'question6', 'question7']

    def is_displayed(self):
        return self.subsession.round_number == 1

class Check(Page):
    def is_displayed(self):
        return self.subsession.round_number == 1

class InstrNet1(Page):
    def is_displayed(self):
        return self.subsession.round_number == 1

    def before_next_page(self):
        self.player.expiration_timestamp = time.time() + 20


class PractiseInstructions(Page):
    def is_displayed(self):
        return self.subsession.round_number == 1


class Decide(Page):
    form_model = models.Player
    form_fields = ['Blue']


class ResultsWaitPage(WaitPage):
    def after_all_players_arrive(self):
        for p in self.group.get_players():
            p.set_payoff()

    body_text = "Waiting for the other participants."


class Results(Page):
    pass


page_sequence = [InstrNet1,
                 Decide,
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