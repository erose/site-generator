import datetime, itertools, glob, os
from os.path import join
import markdown, bs4

def ord_name(day):
    """ Takes in a day of the month, gives back '3rd', '15th', etc. """
    if 4 <= day <= 20 or 24 <= day <= 30:
        suffix = "th"
    else:
        suffix = ["st", "nd", "rd"][day % 10 - 1]
    return str(day) + suffix

def first_n_tags(htmlstring, n):
    """ Returns a new htmlstring composed of the first n top-level tags. """
    soup = bs4.BeautifulSoup(htmlstring, "html.parser")
    return "".join(str(tag) for tag in soup.contents[:n])

def clean_folder(path, pattern="*"):
    for path in glob.glob(join(path, pattern)):
        os.remove(path)

def parse_date(datestring):
    return datetime.datetime.strptime(datestring, "%Y-%m-%d") # 2016-02-22

def format_date(dt):
    return dt.strftime(
        "%A, %B {}, %Y".format(ord_name(dt.day)) # Monday, February 22nd, 2016
    )

def parse_tags(tagstring):
    return [dict(name=tag) for tag in tagstring.split(", ")]

def parse_content(string):
    return markdown.markdown(string)

def parse_md(file_obj):
    """ Separates options from content in an .md file. Returns as a dict. """
    result = {'name': os.path.basename(file_obj.name)}

    # A single blank line separates the options and the content.
    not_blank = lambda s: s.rstrip() != ''
    for line in itertools.takewhile(not_blank, file_obj):
        key, value = line.rstrip().split(": ")

        if key == "Date":
            dt = parse_date(value)
            result["formatted_date"] = format_date(dt)
            result["date"] = dt
        elif key == "Tags":
            result["tags"] = parse_tags(value)
        elif key == "Title":
            result["title"] = value
        elif key == "Status":
            result["status"] = value
        else:
            pass # Ignore other options silently.

    result["content"] = parse_content("".join(file_obj))
    return result
