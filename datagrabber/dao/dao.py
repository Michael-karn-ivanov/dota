__author__ = 'mivanov'

class Dao:

    def save_team_list(self, teams):
        team_file = open('teams.csv', 'w')
        for team in teams:
            team_file.write("{0},{1},{2}\n".format(team._id, team._name, team._href))

    def save_match_raw(self, match):
        with open('matches.csv', 'a') as match_file:
            match_file.write("{0}\t{1}\t{2}\t{3}\t{4}\t{5}\t{6}\n".format(
                match._id, match._date, match._radiant, match._dire, match._winner, match._firstblood, match._league))

    def save_match_list(self, matches):
        match_file = open('matches.csv', 'w')
        for match in matches:
            match_file.write("{0}\t{1}\t{2}\t{3}\t{4}\t{5}\t{6}\n".format(
                match._id, match._date, match._radiant, match._dire, match._winner, match._firstblood, match._league))

    def save_league_list(self, leagues):
        league_file = open('leagues.csv', 'w')
        for league in leagues:
            league_file.write("{0}\t{1}\t{2}\t{3}\n".format(league._id, league._name, league._type, league._prizepool))

