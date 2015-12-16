import httplib
import re
import time
import sys
from model.team import Team
from model.match import Match
from model.league import League
__author__ = 'mivanov'

class TeamProvider:
    _root_url = "http://www.dotabuff.com/esports/teams"
    _conn = httplib.HTTPConnection('www.dotabuff.com')

    def invoke_url(self, url):
        while True:
            try:
                self._conn.request('GET', url, headers={ 'User-Agent' : 'super happy flair bot by /u/spladug' })
                data = self._conn.getresponse().read()
                if '<title>DOTABUFF - Too Many Requests</title>' in data:
                    print "Dotabuff is tired, lets wait 3 minuttes and retry"
                    time.sleep(180)
                    print "retry..."
                    self._conn = httplib.HTTPConnection('www.dotabuff.com')
                else:
                    return data
            except:
                print "Unexpected error:", sys.exc_info()[0], " when requesting ", url, ", sleep for 3 minutes"
                time.sleep(180)
                print "retry..."
                self._conn = httplib.HTTPConnection('www.dotabuff.com')


    def get_team_list(self):
        teams = []
        team_file = open('teams.html', 'r')
        team_page = team_file.read()
        m = re.findall('<tr><td data-value=""><a href="(http://www.dotabuff.com/esports/teams/(\d+))"><img alt="([^"]+)" class="img-team img-avatar" onerror=', team_page)
        for i in m:
            teams.append(Team(i[1], i[2], i[0]))
        return teams

    def get_matches_for_team(self, team_id):
        matches = []

        for i in range(1, 10):
            data = self.invoke_url('/esports/teams/' + team_id + '/matches?page=' + str(i))
            mwon = re.findall('<a class="won" href="/matches/(\d+)">Won Match</a>', data)
            mlost = re.findall('<a class="lost" href="/matches/(\d+)">Lost Match</a>', data)
            print "\t\tpage #", i
            if not mwon and not mlost:
                return matches
            for w in mwon:
                matches.append(Match(w))
            for l in mlost:
                matches.append(Match(l))
        return matches

    def load_match_info(self, match):
        data = self.invoke_url('/matches/' + match._id)
        mleague = re.findall('<a class="esports-link" href="/esports/leagues/(\d+)">', data)
        match._league = mleague[0]
        mdate = re.findall('<dt>Duration</dt></dl><dl><dd><time datetime="(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2})\+\d{2}:\d{2}"', data)
        match._date = mdate[0]
        mradiant = re.findall('<section class="radiant"><header style="vertical-align: middle"><a href="/esports/teams/(\d+)', data)
        mdire = re.findall('<section class="dire"><header style="vertical-align: middle"><a href="/esports/teams/(\d+)', data)
        if not mradiant:
            match._radiant = 'NA'
        else:
            match._radiant = mradiant[0]
        if not mdire:
            match._dire = 'NA'
        else:
            match._dire = mdire[0]
        if '<div class="match-result team radiant">Radiant Victory</div><div class="team-results">' in data:
            match._winner = 'radiant'
        elif '<div class="match-result team dire">Dire Victory</div><div class="team-results">' in data:
            match._winner = 'dire'
        else:
            raise ValueError('winner undefined')
        data = self.invoke_url('/matches/' + match._id + '/kills')
        if '<h2 id="status">Not Found</h2>' in data:
            match._firstblood = 'NA'
        else:
            if '<div class="death-event death-event-dire"' not in data and '<div class="death-event death-event-radiant"' not in data:
                match._firstblood = 'NA'
            elif '<div class="death-event death-event-dire"' not in data:
                match._firstblood = 'radiant'
            elif '<div class="death-event death-event-radiant"' not in data:
                match._firstblood = 'dire'
            else:
                if data.index('<div class="death-event death-event-dire"') > data.index('<div class="death-event death-event-radiant"'):
                    match._firstblood = 'dire'
                else:
                    match._firstblood = 'radiant'

    def load_league_info(self, id):
        data = self.invoke_url('/esports/leagues/' + id)
        mlg = re.findall('<div class="header-content-title"><h1>([^<]+)<small>([^<]+)</small></h1></div></div><div class="header-content-secondary"><dl><dd class="prize-money">([^<]+)</dd>', data)
        if not mlg:
            mlg = re.findall('<div class="header-content-title"><h1>([^<]+)<small>([^<]+)</small></h1></div></div>', data)
            return League(id, mlg[0][0],mlg[0][1], 'NA')
        else:
            return League(id, mlg[0][0],mlg[0][1],mlg[0][2])