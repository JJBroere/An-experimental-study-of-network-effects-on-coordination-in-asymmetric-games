from otree.api import (
    models, widgets, BaseConstants, BaseSubsession, BaseGroup, BasePlayer,
    Currency as c, currency_range
)
import random
import numpy as np


doc = """
This is a 2-player 2-strategy coordination game. The name and story originated
from
<a href="http://books.google.ch/books?id=uqDDAgAAQBAJ&lpg=PP1&ots=S-DC4LemnS&lr&pg=PP1#v=onepage&q&f=false" target="_blank">
    Luce and Raiffa (1957)
</a>.
"""


class Constants(BaseConstants):
    name_in_url = 'battle_of_the_sexes'
    players_per_group = 4 #None if everybody is in the same group
    num_rounds = 1

    instructions_template = 'battle_of_the_sexes/Instructions.html'

    mismatch_payoff = c(0)
    CoordinatePref_payoff = c(10)
    CoordinateNotPref_payoff = c(8)

    Adjmat = ([[False, True, True, False], [True, False, True, False], [True, True, False, True],
               [False, False, True, False]])  # to constants?


    alpha = random.sample(range(1,5), k=2)

class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    question1 = models.IntegerField()
    question2 = models.IntegerField()
    question3 = models.IntegerField()
    question4 = models.IntegerField()
    question5 = models.IntegerField()
    question6 = models.IntegerField()
    question7 = models.IntegerField()

    def ansQues1(self):
        if self.question1 == 10:
            return 'Correct! This is indeed the payoff related to you choosing the option related to your type and the ' \
                   'other player choosing this option as well.'
        else:
            return 'Not correct. If you choose the option related to your type and the other player chooses this option ' \
                   'as well, you will get the highest payoff of 10. Please check if you understand why the ' \
                   'payoff is 10, otherwise ask one of supervisors in the rom for clarification'

    def ansQues2(self):
        if self.question2 == 8:
            return 'Correct! If you choose the option that is not related to your type and the other player chooses this' \
                   ' option as well, you will get a payoff of 8.'
        else:
            return 'Not correct. If you choose the option that is not related to your type and the other player chooses this option ' \
                   'as well, you will get a payoff of 8. Please check if you understand why the ' \
                   'payoff is 8, otherwise ask one of supervisors in the rom for clarification'

    def ansQues3(self):
        if self.question3 == 0:
            return 'Correct! When you and the other player choose different option you both get a payoff of 0.'
        else:
            return 'Not correct. When you and the other player choose different option you both get a payoff of 0. Only ' \
                   'when you and the other player choose the same option you will get a payoff, depending on your type ' \
                   'and the chosen option. Please check if you understand why the ' \
                   'payoff is 0, otherwise ask one of supervisors in the room for clarification'

    def ansQues4(self):
        if self.question4 == 10:
            return 'Correct! This is indeed the payoff related to you choosing the option related to your type and the ' \
                   'other player choosing this option as well.'
        else:
            return 'Not correct. If you choose the option related to your type and the other player chooses this option ' \
                   'as well, you will get the highest payoff of 10. Please check if you understand why the ' \
                   'payoff is 10, otherwise ask one of supervisors in the rom for clarification'

    def ansQues5(self):
        if self.question5 == 8:
            return 'Correct! If you choose the option that is not related to your type and the other player chooses this' \
                   ' option as well, you will get a payoff of 8.'
        else:
            return 'Not correct. If you choose the option that is not related to your type and the other player chooses this option ' \
                   'as well, you will get a payoff of 8. Please check if you understand why the ' \
                   'payoff is 8, otherwise ask one of supervisors in the rom for clarification'

    def ansQues6(self):
        if self.question6 == 10:
            return 'Correct! One participant also chooses yellow and for this participant your payoff will be 10. The ' \
                   'other participant chooses the other option, so this will give a payoff of 0. So, 10 + 0 = 10.'
        else:
            return 'Not correct. One participant also chooses yellow and for this participant your payoff will be 10. The ' \
                   'other participant chooses the other option, so this will give a payoff of 0. So, 10 + 0 = 10. Please ' \
                   'check if you understand why the ' \
                   'payoff is 10, otherwise ask one of supervisors in the room for clarification'

    def ansQues7(self):
        if self.question7 == 5:
            return 'Correct! One participant also chooses yellow and for this participant your payoff will be 10. The ' \
                   'other participant chooses the other option, so this will give a payoff of 0. So, (10 + 0)/2 = 5.'
        else:
            return 'Not correct. One participant also chooses yellow and for this participant your payoff will be 10. The ' \
                   'other participant chooses the other option, so this will give a payoff of 0. So, (10 + 0)/2 = 5. Please ' \
                   'check if you understand why the ' \
                   'payoff is 5, otherwise ask one of supervisors in the room for clarification'

    Blue = models.BooleanField(
        choices=[
            [False, 'yellow'],
            [True, 'blue']
        ],
        doc = """Either blue or yellow"""
    )

    def decision_label(self):
        if self.Blue:
            return 'blue'
        return 'yellow'


    degreec = models.IntegerField()
    def degree(self):
        ts = self.id_in_group
        nts = ts - 1
        numpl = sum(Constants.Adjmat[nts])
        self.degreec = sum(Constants.Adjmat[nts])
        return numpl

    color = models.StringField()
    def role(self):
        if self.id_in_group in Constants.alpha[:]:
            self.color = 'blue'
            return 'blue'
        else:
            self.color = 'yellow'
            return 'yellow'

    def set_adj_pla(self):
        ts = self.id_in_group
        nts = ts - 1  # -1 because Python starts indexing at 0, for some reason
        t_oppoinents = Constants.Adjmat[nts]
        et_oppoinents = np.where(t_oppoinents)  # selecting the adjecent players
        set_oppoinents = et_oppoinents[0]
        for n, i in enumerate(
                set_oppoinents):  # vector of adjecent players as indexed by the function self.get_others_in_group()[p] *few lines below
            if i >= self.id_in_group:
                set_oppoinents[n] = set_oppoinents[n] - 1
        return set_oppoinents

    def choises_other_players_F(self):
        set_oppoin = self.set_adj_pla()
        numf = 0
        for i in set_oppoin:
            pl = self.get_others_in_group()[i]
            numf += pl.Blue
        return numf



    def choises_other_players_B(self):
        numb = self.degree() - self.choises_other_players_F()
        return numb

    def set_payoff(self):

        if self.id_in_group in Constants.alpha[:]:
            set_oppoinent = self.set_adj_pla()
            for p in set_oppoinent: #Calculating the payoff, only for the adjecent players
                pla = self.get_others_in_group()[p] #selecting adjecent players from list
                payoff_matrix1 = {
                    True:
                        {
                            True: Constants.CoordinatePref_payoff,
                            False: Constants.mismatch_payoff
                        },
                    False:
                        {
                            True: Constants.mismatch_payoff,
                            False: Constants.CoordinateNotPref_payoff
                        }
                }
                payoff = payoff_matrix1[self.Blue][pla.Blue]
                self.payoff += payoff

        else:
            set_oppoinent = self.set_adj_pla()
            for p in set_oppoinent:
                pla = self.get_others_in_group()[p]
                payoff_matrix2 = {
                    True:
                        {
                            True: Constants.CoordinateNotPref_payoff,
                            False: Constants.mismatch_payoff
                        },
                    False:
                        {
                            True: Constants.mismatch_payoff,
                            False: Constants.CoordinatePref_payoff
                        }
                }
                payoff = payoff_matrix2[self.Blue][pla.Blue]
                self.payoff += payoff


    def points(self):
        pyt = float(self.payoff)
        pyd =float(self.degree())
        average = round(pyt/pyd, 2)
        return(average)