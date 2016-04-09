import httplib
import time
import json
import sys
import os.path
from model.coeff import Coeff

__author__ = 'mivanov'


class CoeffProvider:
    _conn = httplib.HTTPConnection('egb.com')

    def get_coeff_list(self):
        response = None
        coeffs = []
        if os.path.isfile('data.json'):
            with open('data.json') as data_file:
                response = json.load(data_file)
        else:
            data = self.invoke_url('/ajax.php?act=UpdateTableBets&ajax=update&fg=1&ind=tables&limit=0&st=0&type=modules&ut=0')
            response = json.loads(data)
        bets = response['bets']
        for bet in bets:
            if ((bet['game'] != 'Dota2') & (bet['game'] != 'LoL') & (bet['game'] != 'Counter-Strike')):
                continue
            game = bet['game']
	    team1 = bet['gamer_1']['nick'].encode('utf-8')
            team2 = bet['gamer_2']['nick'].encode('utf-8')
            result = ''
            if bet['gamer_1']['win'] == 1 and bet['gamer_2']['win'] == 0:
                result = 'win1'
            elif bet['gamer_1']['win'] == 0 and bet['gamer_2']['win'] == 1:
                result = 'win2'
            elif bet['gamer_1']['win'] == 0 and bet['gamer_2']['win'] == 0:
                result = 'draw'
            else:
                result = 'NA'
            coeffs.append(Coeff(bet['id'], game, bet['date'], bet['coef_1'], bet['coef_2'], team1, team2, 'Total', bet['tourn'], bet['id'], 'NA', result))
            if 'nb_arr' in bet:
                nested_bets = bet['nb_arr']
                for nested in nested_bets:
                    bet_type = 'NA'
                    map = 'NA'
                    result = ''
                    if nested['gamer_1']['win'] == 1 and nested['gamer_2']['win'] == 0:
                        result = 'win1'
                    elif nested['gamer_1']['win'] == 0 and nested['gamer_2']['win'] == 1:
                        result = 'win2'
                    elif nested['gamer_1']['win'] == 0 and nested['gamer_2']['win'] == 0:
                        result = 'draw'
                    else:
                        result = 'NA'
                    if nested['gamer_1']['nick'] == 'Map 1':
                        bet_type = 'GameResult'
                        map = 'Map1'
                    elif nested['gamer_1']['nick'].lower() == 'first blood on map 1':
                        bet_type = 'FB'
                        map = 'Map1'
                    elif nested['gamer_1']['nick'].lower() == 'will take first 10 kills on map 1':
                        bet_type = 'Kills10'
                        map = 'Map1'
                    elif nested['gamer_1']['nick'] == 'Map 2':
                        bet_type = 'GameResult'
                        map = 'Map2'
                    elif nested['gamer_1']['nick'].lower() == 'first blood on map 2':
                        bet_type = 'FB'
                        map = 'Map2'
                    elif nested['gamer_1']['nick'].lower() == 'will take first 10 kills on map 2':
                        bet_type = 'Kills10'
                        map = 'Map2'
                    elif nested['gamer_1']['nick'] == 'Map 3':
                        bet_type = 'GameResult'
                        map = 'Map3'
                    elif nested['gamer_1']['nick'].lower() == 'first blood on map 3':
                        bet_type = 'FB'
                        map = 'Map3'
                    elif nested['gamer_1']['nick'].lower() == 'will take first 10 kills on map 3':
                        bet_type = 'Kills10'
                        map = 'Map3'
                    else:
                        bet_type = nested['gamer_1']['nick']
                        map = nested['gamer_2']['nick']
                    coeffs.append(Coeff(nested['id'], game, nested['date'], nested['coef_1'], nested['coef_2'], team1, team2, bet_type, bet['tourn'], bet['id'], map, result))
        return coeffs


    def invoke_url(self, url):
        while True:
            try:
                self._conn.request('GET', url, headers={ 'User-Agent' : 'super happy flair bot by /u/spladug' })
                data = self._conn.getresponse().read()
                return data
            except:
                print "Unexpected error:", sys.exc_info()[0], " when requesting ", url, ", sleep for 3 minutes"
                time.sleep(180)
                print "retry..."
                self._conn = httplib.HTTPConnection('www.dotabuff.com')


