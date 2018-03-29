import untangle
import csv

# Scraper that fetches information about votes in congress

logging = False
writeResults = True

outputFile = '../data/votes.csv'
results = [["vote_id", "congress", "topic_id", "topic", "time", "member_id", "vote"]]
votingNumbers = []
totalVoteData = 0
failed = 0

# fetching voting numbers
if (logging):
    print("Fetching voting numbers")
for congress in range(148, 149):
    obj = untangle.parse('http://www.althingi.is/altext/xml/atkvaedagreidslur/?lthing=' + str(congress))
    for vote in obj.atkvæðagreiðslur.children:
        votingNumbers.append(vote['atkvæðagreiðslunúmer'])
    if (logging):
        print("Finished fetching vote numbers for congress " + str(congress))

if (logging):
    print("Finished fetching voting numbers")

for vote in votingNumbers:
    obj = untangle.parse('http://www.althingi.is/altext/xml/atkvaedagreidslur/atkvaedagreidsla/?numer=' + str(vote))
    try:
        for member in obj.atkvæðagreiðsla.atkvæðaskrá.children:
            results.append([
                obj.atkvæðagreiðsla['atkvæðagreiðslunúmer'],
                obj.atkvæðagreiðsla['þingnúmer'],
                obj.atkvæðagreiðsla['málsnúmer'],
                obj.atkvæðagreiðsla.mál.málsheiti.cdata,
                obj.atkvæðagreiðsla.tími.cdata,
                member['id'],
                member.atkvæði.cdata
                ])
            totalVoteData += 1
    except AttributeError as e:
        failed += 1
        if (logging):
            print("Could not fetch data on vote " + str(vote))

print("Finished fetching data on " + str(totalVoteData) + " failed fetching data for " + str(failed) + " votes.")

if(writeResults):
    print('Writing to ' + outputFile)
    summaryCSV = open(outputFile, 'w', encoding="utf-8")
    with summaryCSV:
        writer = csv.writer(summaryCSV)
        writer.writerows(results)
    print('Finished writing data to ' + outputFile)