import untangle
import csv
from dateutil.parser import parse

logging = False

outputFile = '../data/summary.csv'
results = [["vote_id", "congress",
            "vote_time_year",
            "vote_time_month",
            "vote_time_date",
            "vote_time_hour",
            "vote_time_minute",
            "vote_time_second",
            "yes", "no", "did not vote"]] #schema for csv file
for congress in range(115, 149):
    obj = untangle.parse('http://www.althingi.is/altext/xml/atkvaedagreidslur/?lthing=' + str(congress))
    if(logging):
        print("Starting to fetch data for congress " + str(congress))
    atkvaedagreidslukerfi = 0
    other = 0
    failed = 0
    for athvaedagreidsla in obj.atkvæðagreiðslur.atkvæðagreiðsla:
        try:
            if athvaedagreidsla.samantekt.aðferð.cdata == "atkvæðagreiðslukerfi":
                vote_time = parse(athvaedagreidsla.tími.cdata)
                results.append([athvaedagreidsla['atkvæðagreiðslunúmer'], congress,
                            vote_time.strftime('%Y'),
                            vote_time.strftime('%m'),
                            vote_time.strftime('%d'),
                            vote_time.strftime('%H'),
                            vote_time.strftime('%M'),
                            vote_time.strftime('%S'),
                            athvaedagreidsla.samantekt.já.fjöldi.cdata,
                            athvaedagreidsla.samantekt.nei.fjöldi.cdata,
                            athvaedagreidsla.samantekt.greiðirekkiatkvæði.fjöldi.cdata]) #write to csv
                if(logging):
                    print("Results for vote " + athvaedagreidsla['atkvæðagreiðslunúmer'] + " on " + athvaedagreidsla.tími.cdata)
                    print("já: " + athvaedagreidsla.samantekt.já.fjöldi.cdata)
                    print("nei: " + athvaedagreidsla.samantekt.nei.fjöldi.cdata)
                    print("greiðir ekki atkvæði: " + athvaedagreidsla.samantekt.greiðirekkiatkvæði.fjöldi.cdata)
                atkvaedagreidslukerfi += 1
            else:
                other += 1
        except AttributeError as e:
            if(logging):
                print("Could not fetch results for vote " + athvaedagreidsla['atkvæðagreiðslunúmer'])
                print(e)
            failed += 1
    if(logging):
        print("Finished fetching results from votes in congress " + str(congress))
        print("Successfully fetched summaries for " + str(atkvaedagreidslukerfi) + " votes in atkvæðagreiðslukerfi")
        print("There vere " + str(other) + " other votes, which data was not collected on.")
        print("Data collection failed for " + str(failed) + " votes.")
    print("Finished collecting data for Þing " + str(congress))
    print('Data: ' + str(atkvaedagreidslukerfi) + ', skipped: ' + str(other) + ', failed: ' + str(failed))

print('Writing to ' + outputFile)
summaryCSV = open(outputFile, 'w', encoding="utf-8")
with summaryCSV:
    writer = csv.writer(summaryCSV)
    writer.writerows(results)
print('Finished writing data to ' + outputFile)