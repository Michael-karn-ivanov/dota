import datetime

__author__ = 'mivanov'

class Dao:

    def save_coeff_list(self, coeffs, fileName = None):
        if fileName is None:
            now = datetime.datetime.now()
            fileName = 'coeffs-{0}-{1}-{2}-{3}-{4}.csv'.format(now.year, now.month, \
                                                                       now.day if now.day > 10 else "0" + `now.day`, \
                                                                       now.hour if now.hour > 10 else "0" + `now.hour`, \
                                                                       now.minute if now.minute > 10 else "0" + `now.minute`,
                                                                       now.second if now.second > 10 else "0" + `now.second`)
        coeff_file = open('Data/' + fileName, 'w')
        for coeff in coeffs:
            coeff_file.write("{0},{1},{2},{3},{4},{5},{6},{7},{8},{9},{10},{11}\n".format(coeff._id, coeff._game, datetime.datetime.fromtimestamp(int(coeff._date)).strftime('%Y-%m-%d %H:%M:%S.%f'), coeff._coeff1, coeff._coeff2, coeff._team1, coeff._team2, coeff._type, coeff._tournament, coeff._map, coeff._parentid, coeff._result))