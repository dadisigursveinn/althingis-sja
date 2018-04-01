import untangle
import csv
from dateutil.parser import parse

# Scraper that fetches information about votes in congress

logging = True
writeResults = True

outputdir = '../data/votes/'
results = [["vote_id", "congress", "topic_id", "topic",
            "vote_time_year",
            "vote_time_month",
            "vote_time_date",
            "vote_time_hour",
            "vote_time_minute",
            "vote_time_second",
            "member_id", "vote"]]
totalVoteData = 0
failed = 0
lastPrct = 0
prctIncrement = 5

def collectVoteInfo (votingNumbers, congress):
    for indx, vote in enumerate(votingNumbers):
        obj = untangle.parse('http://www.althingi.is/altext/xml/atkvaedagreidslur/atkvaedagreidsla/?numer=' + str(vote))
        if logging:
            print("fetching data for " + str(indx) + " of " + str(len(votingNumbers)))
        try:
            for member in obj.atkvæðagreiðsla.atkvæðaskrá.children:
                vote_time = parse(obj.atkvæðagreiðsla.tími.cdata)

                results.append([
                    obj.atkvæðagreiðsla['atkvæðagreiðslunúmer'],
                    obj.atkvæðagreiðsla['þingnúmer'],
                    obj.atkvæðagreiðsla['málsnúmer'],
                    obj.atkvæðagreiðsla.mál.málsheiti.cdata,
                    vote_time.strftime('%Y'),
                    vote_time.strftime('%m'),
                    vote_time.strftime('%d'),
                    vote_time.strftime('%H'),
                    vote_time.strftime('%M'),
                    vote_time.strftime('%S'),
                    member['id'],
                    member.atkvæði.cdata
                    ])
            global totalVoteData
            totalVoteData += 1
            if (logging and False):
                print("Fetched data data on vote " + str(vote))
        except AttributeError as e:
            global failed
            failed += 1
            if (logging and False):
                print("Could not fetch data on vote " + str(vote))
                print(e)
    if(writeResults):
        outputFile = outputdir + 'votes_' + str(congress)
        print('Writing to ' + outputFile)
        summaryCSV = open(outputFile, 'w', encoding="utf-8")
        with summaryCSV:
            writer = csv.writer(summaryCSV)
            writer.writerows(results)
        print('Finished writing data to ' + outputFile)
        summaryCSV.close()

# fetching voting numbers
if (logging):
    print("Fetching voting numbers")
for congress in range(128, 149):
    votingNumbers = []
    obj = untangle.parse('http://www.althingi.is/altext/xml/atkvaedagreidslur/?lthing=' + str(congress))
    for vote in obj.atkvæðagreiðslur.children:
        votingNumbers.append(vote['atkvæðagreiðslunúmer'])
    if (logging):
        print("Finished fetching vote numbers for congress " + str(congress))
    collectVoteInfo(votingNumbers, congress)

if (logging):
    print("Finished fetching voting numbers")

print("Finished fetching data on " + str(totalVoteData) + " failed fetching data for " + str(failed) + " votes.")