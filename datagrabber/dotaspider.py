import sys

__author__ = 'mivanov'
from providers.teamprovider import TeamProvider
from model.team import Team
from model.match import Match
from dao.dao import Dao

teamprovider = TeamProvider()
dao = Dao()

#teams = teamprovider.get_team_list()
#dao.save_team_list(teams)
#
# matches = []
# counter = 0
# for team in teams:
#     counter = counter + 1
#     if counter < 50:
#         continue
#     print "processing ", counter, " out of ", len(teams)
#     try:
#         team_matches = teamprovider.get_matches_for_team(team._id)
#         if not team_matches:
#             print "Match list is empty for ", team._id
#         subcounter = 0
#         for match in team_matches:
#             subcounter = subcounter + 1
#             print "Team: ", counter, " / ", len(teams), ", match: ", subcounter, " / ", len(team_matches)
#             try:
#                 teamprovider.load_match_info(match)
#                 dao.save_match_raw(match)
#             except:
#                 print "Unexpected error:", sys.exc_info()[0], " while load match info, team: ", team._id, ", match: ", match._id
#         matches.extend(team_matches)
#     except:
#         print "Unexpected error:", sys.exc_info()[0], " while load team matches, team: ", team._id
#
# print "start aggregating league information"
# leagues_id = set([])
# for match in matches:
#     if hasattr(match, '_league'):
#         if match._league not in leagues_id:
#             leagues_id.add(match._league)
#
# print "gather league info"
# leagues = []
# for id in leagues_id:
#     league = teamprovider.load_league_info(id)
#     leagues.append(league)
# dao.save_league_list(leagues)