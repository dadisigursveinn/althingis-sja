import untangle
import csv

# Scraper that fetches information about members of congress
# id, name, dob,  initials
# writes the information to a csv file

logging = True
writeResults = True

outputFile = '../data/members.csv'
results = [["member_id", "name", "dob"]] #schema for csv file
obj = untangle.parse('http://www.althingi.is/altext/xml/thingmenn/')
if(logging):
    print("Starting to fetch data")
congressmen = 0
failed = 0

for thingmadur in obj.þingmannalisti.children:
    #print(thingmadur)
    results.append([thingmadur['id'], thingmadur.nafn.cdata, thingmadur.fæðingardagur.cdata])
    congressmen += 1

if(logging):
    print("Finished fetching data")
    print("Fetched " + str(congressmen) + " members of congress.")

if(writeResults):
    print('Writing to ' + outputFile)
    summaryCSV = open(outputFile, 'w', encoding="utf-8")
    with summaryCSV:
        writer = csv.writer(summaryCSV)
        writer.writerows(results)
    print('Finished writing data to ' + outputFile)