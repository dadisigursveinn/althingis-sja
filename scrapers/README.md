# Scrapers
## member_detail.py 
Creates a *members_details.csv* file containing information on members of parliament.

| id | name | session | type | party_id | constituency | started | ended |
|----|------|---------|------|----------|--------------|---------|-------|
|    |      |         |      |          |              |         |       |

## members.py
Creates a *members.csv* file containing a list of all members of parliment.


| id | name | dob |
|----|------|-----|
|    |      |     |

## members_by_congress_scraper.py
Creates a *members_by_congress.csv* file containing the members of parliment for each congress.

| id | name | dob | initials | session |
|----|------|-----|----------|---------|
|    |      |     |          |         |

## summary_scraper.py
Creates a *summary.csv* file containing a summary of all votes in parliment.

| vote_id | session | time | yes | no | did not vote |
|---------|---------|------|-----|----|--------------|
|         |         |      |     |    |              |

## votes_scraper.py
Creates a *votes.csv* file containing information about how each member of parliment voted in every vote.

| vote_id | session | matter_id | matter | time | member_id | vote |
|---------|---------|-----------|--------|------|-----------|------|
|         |         |           |        |      |           |      |

# How to use
Run
```
python scraper_name.py
```
the scraper will run and generate a `.csv` file for you.
