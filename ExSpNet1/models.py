from __future__ import division

import random

from otree.constants import BaseConstants
from otree.models import BaseSubsession, BaseGroup, BasePlayer, BaseLink

from otree.db import models
from otree import widgets
from otree.common import Currency as c, currency_range
import numpy as np
doc = """
This is a 2-player 2-strategy coordination game. The name and story originated
from
<a href="http://books.google.ch/books?id=uqDDAgAAQBAJ&lpg=PP1&ots=S-DC4LemnS&lr&pg=PP1#v=onepage&q&f=false" target="_blank">
    Luce and Raiffa (1957)
</a>.
"""


class Constants(BaseConstants):
    name_in_url = 'ExperimentPart1'
    players_per_group = None #None if everybody is in the same group
    num_rounds = 20

    instructions_template = 'ExSpNet1/Instructions.html'

    mismatch_payoff = c(0)
    CoordinatePref_payoff = c(10)
    CoordinateNotPref_payoff = c(8)


    Adjmat = ([[False, True, False, False, False, False, False, False, True, False, False, False, False, False, False, False, False, False, False, False],
               [True, False, False, False, True, False, True, True, False, False, False, False, False, False, False, False, True, False, False, False],
               [False, False, False, False, False, False, False, False, False, False, True, False, False, False, True, False, True, False, False, False],
               [False, False, False, False, False, False, False, True, True, False, False, False, True, False, False, False, False, False, False, False],
               [False, True, False, False, False, False, False, False, False, False, False, False, False, True, False, False, True, False, True, False],
               [False, False, False, False, False, False, False, False, False, False, False, True, False, False, False, False, False, False, False, True],
               [False, True, False, False, False, False, False, False, False, True, False, False, False, False, True, False, False, False, True, False],
               [False, True, False, True, False, False, False, False, False, False, False, True, False, False, False, True, False, False, False, False],
               [True, False, False, True, False, False, False, False, False, True, False, False, True, False, False, True, False, False, False, True],
               [False, False, False, False, False, False, True, False, True, False, True, False, True, False, False, True, True, False, False, False],
               [False, False, True, False, False, False, False, False, False, True, False, False, True, True, False, False, False, False, False, False],
               [False, False, False, False, False, True, False, True, False, False, False, False, True, False, True, False, False, False, False, False],
               [False, False, False, True, False, False, False, False, True, True, True, True, False, False, False, False, True, False, False, False],
               [False, False, False, False, True, False, False, False, False, False, True, False, False, False, False, False, False, False, False, False],
               [False, False, True, False, False, False, True, False, False, False, False, True, False, False, False, False, False, True, False, False],
               [False, False, False, False, False, False, False, True, True, True, False, False, False, False, False, False, True, False, True, False],
               [False, True, True, False, True, False, False, False, False, True, False, False, True, False, False, True, False, True, False, True],
               [False, False, False, False, False, False, False, False, False, False, False, False, False, False, True, False, True, False, False, False],
               [False, False, False, False, True, False, True, False, False, False, False, False, False, False, False, True, False, False, False, False],
               [False, False, False, False, False, True, False, False, True, False, False, False, False, False, False, False, True, False, False, False]])  # to constants?
    '''
    alpha = random.sample(range(1, 21), k=10)
    

    Adjmat = ([[False, True, True, False], [True, False, True, False], [True, True, False, True],
               [False, False, True, False]])  # to constants?
    '''




class Subsession(BaseSubsession):
    alpha = [3, 6, 17, 1, 11, 4, 10, 8, 12, 15]


class Group(BaseGroup):
    pass

class Link(BaseLink):
    pass

class Player(BasePlayer):
    question1 = models.IntegerField()
    question2 = models.IntegerField()
    question3 = models.IntegerField()
    question4 = models.IntegerField()
    question5 = models.IntegerField()
    question6 = models.IntegerField()
    question7 = models.IntegerField()
    expiration_timestamp = models.FloatField()

    def ansQues1(self):
        if self.question1 == 10:
            return 'Correcto! Este es, de hecho, el pago relacionado con la elección de la opción relacionada con su tipo' \
                   ' y el otro jugador que elige esta opción también.'
        else:
            return 'No es correcto. Si eliges la opción relacionada con tu tipo y el otro jugador elige esta opción también,' \
                   ' obtendrás el pago más alto de 10. Por favor, compruebe si entiende por qué el pago es de 10, de lo ' \
                   'contrario pregunte a uno de los supervisores en la rom para aclarar.'

    def ansQues2(self):
        if self.question2 == 8:
            return 'Correcto! Si eliges la opción que no está relacionada con tu tipo y el otro jugador elige esta opción ' \
                   'también, obtendrás un resultado final de 8.'
        else:
            return 'No es correcto. Si eliges la opción que no está relacionada con tu tipo y el otro jugador elige esta' \
                   ' opción también, obtendrás un resultado final de 8. Por favor, compruebe si entiende por qué el pago' \
                   ' es de 8, de lo contrario, pida a uno de los supervisores en la rom para aclarar.'

    def ansQues3(self):
        if self.question3 == 0:
            return 'Correcto! Cuando usted y el otro jugador eligen una opción diferente, ambos obtienen una recompensa de 0.'
        else:
            return 'No es correcto. Cuando usted y el otro jugador eligen una opción diferente, ambos obtienen una recompensa' \
                   ' de 0. Sólo cuando tú y el otro jugador elijáis la misma opción obtendréis una recompensa, dependiendo' \
                   ' de vuestro tipo y de la opción elegida. Por favor marque si entiende por qué el pago es 0, de lo ' \
                   'contrario pregunte a uno de los supervisores en la sala por aclaraciones'

    def ansQues4(self):
        if self.question4 == 10:
            return 'Correcto! Este es, de hecho, el pago relacionado con la elección de la opción relacionada con su tipo' \
                   'y el otro jugador que elige esta opción también.'
        else:
            return 'No es correcto. Si eliges la opción relacionada con tu tipo y el otro jugador elige esta opción también, ' \
                   'obtendrás el pago más alto de 10. Por favor, compruebe si entiende por qué el pago es de 10, de lo contrario,' \
                   ' pida a uno de los supervisores en la rom para aclarar.'

    def ansQues5(self):
        if self.question5 == 8:
            return 'Correcto! Si eliges la opción que no está relacionada con tu tipo y el otro jugador elige esta opción' \
                   ' también, obtendrás un resultado final de 8.'
        else:
            return 'No es correcto. Si eliges la opción que no está relacionada con tu tipo y el otro jugador elige esta' \
                   ' opción también, obtendrás un resultado final de 8. Por favor, compruebe si entiende por qué el pago' \
                   ' es de 8, de lo contrario, pida a uno de los supervisores en la rom para aclarar.'

    def ansQues6(self):
        if self.question6 == 10:
            return 'Correcto! Un participante también elige amarillo y para este participante su pago será de 10. El otro' \
                   ' participante elige la otra opción, así que esto dará un resultado final de 0. Entonces, 10 + 0 = 10.'
        else:
            return 'No es correcto. Un participante también elige amarillo y para este participante su pago será de 10. ' \
                   'El otro participante elige la otra opción, así que esto dará un resultado final de 0. Entonces, 10 +' \
                   ' 0 = 10. Por favor marque si entiende por qué el pago es de 10, de lo contrario pregunte a uno de los' \
                   ' supervisores en la sala de aclaraciones.'

    def ansQues7(self):
        if self.question7 == 5:
            return 'Correcto! Un participante también elige amarillo y para este participante su pago será de 10. El otro' \
                   ' participante elige la otra opción, así que esto dará un resultado final de 0. Entonces, (10 + 0)/2 = 5.'
        else:
            return 'No es correcto. Un participante también elige amarillo y para este participante su pago será de 10. ' \
                   'El otro participante elige la otra opción, así que esto dará un resultado final de 0. Entonces, (10 ' \
                   '+ 0)/2 = 5. Por favor marque si entiende por qué el pago es de 5, de lo contrario pregunte a uno de ' \
                   'los supervisores en la sala de aclaraciones.'


    Blue = models.BooleanField(
        choices=[
            [False, 'amarillo'],
            [True, 'azul']
        ],
        doc = """Either blue or yellow"""
    )

    def decision_label(self):
        if self.Blue:
            return 'azul'
        return 'amarillo'


    degreec = models.IntegerField()
    def degree(self):
        ts = self.id_in_group
        nts = ts - 1
        numpl = sum(Constants.Adjmat[nts])
        self.degreec = sum(Constants.Adjmat[nts])
        return numpl

    color = models.CharField()
    def role(self):
        if self.id_in_group in Subsession.alpha[:]:
            self.color = 'Azul'
            return 'Azul'
        else:
            self.color = 'Amarillo'
            return 'Amarillo'

    def pp1(self):
        if self.decision_label() == 'amarillo':
            return 0
        elif self.id_in_group in Subsession.alpha[:]:
            return 10
        else:
            return 8

    def pp2(self):
        if self.decision_label() == 'azul':
            return 0
        elif self.id_in_group in Subsession.alpha[:]:
            return 8
        else:
            return 10

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

        if self.id_in_group in Subsession.alpha[:]:
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

    ave = models.FloatField()
    def points(self):
        pyt = float(self.payoff)
        pyd =float(self.degree())
        self.ave = round(pyt / pyd, 2)
        average = round(pyt/pyd, 2)
        return(average)

    totpay = models.FloatField()
    def cpay(self):
        self.totpay = sum([p.ave for p in self.in_all_rounds()])
        self.participant.vars['pay1'] = self.totpay
        cumulative_payoff = sum([p.ave for p in self.in_all_rounds()])
        return(cumulative_payoff)
