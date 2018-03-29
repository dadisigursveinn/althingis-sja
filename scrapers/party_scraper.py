import untangle
import csv

# Scraper that fetches information about parties

logging = False
writeResults = True

outputFile = '../data/parties.csv'
results = [["party_id", "name", "abr_short", "abr_long"]] #schema for csv file
obj = untangle.parse('http://www.althingi.is/altext/xml/thingflokkar/')
if(logging):
    print("Starting to fetch data")

for party in obj.þingflokkar.children:
    #print(party.children)
    results.append([
        party['id'],
        party.heiti.cdata.strip(),
        party.skammstafanir.stuttskammstöfun.cdata.strip(),
        party.skammstafanir.löngskammstöfun.cdata.strip(),
        ])
#print(results)
if(logging):
    print("Finished fetching data")

if(writeResults):
    print('Writing to ' + outputFile)
    summaryCSV = open(outputFile, 'w', encoding="utf-8")
    with summaryCSV:
        writer = csv.writer(summaryCSV)
        writer.writerows(results)
    print('Finished writing data to ' + outputFile)