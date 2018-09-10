from . import pages
from otree.api import Bot

class PlayerBot(Bot):
    def play_round(self):
        assert self.player.Football == 1
        yield (pages.Deside, {'Football': 1})
        yield (pages.Results)

    '''
    
    cases = ['both_football', 'mismatch']

    def play_round(self):
        yield (views.Introduction)

        if self.case == 'both_football':
            yield (views.Decide, {"decision": 'Football'})
            if self.player.role() == 'husband':
                assert self.player.payoff == Constants.football_husband_payoff
            else:
                assert self.player.payoff == Constants.football_wife_payoff

        if self.case == 'mismatch':
            if self.player.role() == 'husband':
                yield (views.Decide, {"decision": 'Football'})
            else:
                yield (views.Decide, {"decision": 'Opera'})
            assert self.player.payoff == Constants.mismatch_payoff

        yield (views.Results)
    '''