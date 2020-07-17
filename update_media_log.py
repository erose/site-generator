import json
import datetime
import contextlib
import readline # This line causes it to be implicitly used by input(). It provides better line-editing and history functionality.

DATE_FORMAT_STRING = "%B %-d, %Y" # e.g. October 23, 2019
DATE_FORMAT_STRING_FOR_STRPTIME = DATE_FORMAT_STRING.replace("%-d", "%d") # %-d (not-zero-padded decimal) is allowed for strftime but not strptime.

class MediaLogItem:
  ALBUM = 'album'
  NOVEL = 'novel'
  ESSAYS = 'essays'
  NONFICTION = 'nonfiction'
  ACADEMIC_PAPER = 'academic paper'
  SHORT_STORIES = 'short stories'
  SHORT_STORY = 'short story'
  MOVIE = 'movie'
  TV_SHOW = 'tv show'
  POETRY = 'poetry'
  WEBCOMIC = 'webcomic'
  COMIC = 'comic'

  TYPES = [
    ALBUM,
    NOVEL,
    ESSAYS,
    NONFICTION,
    ACADEMIC_PAPER,
    SHORT_STORIES,
    SHORT_STORY,
    MOVIE,
    TV_SHOW,
    POETRY,
    WEBCOMIC,
    COMIC,
  ]

  def __init__(self, *, title, type, author, date_completed):
    if type not in MediaLogItem.TYPES:
      raise ValueError("{} is not recognized.".format(type))

    self.title = title
    self.type = type
    self.author = author
    self.date_completed = datetime.datetime.strptime(date_completed, DATE_FORMAT_STRING_FOR_STRPTIME)

  # @return [Dictionary]
  def as_json(self):
    return {
      'title': self.title,
      'type': self.type,
      'author': self.author,
      'date_completed': self.date_completed.strftime(DATE_FORMAT_STRING),
    }

  # @return [None]
  def pretty_printed(self):
    return MediaLogItemJsonEncoder().encode(self.as_json())

class MediaLogItemJsonEncoder(json.JSONEncoder):
  def __init__(self):
    super().__init__(indent=4, sort_keys=True, ensure_ascii=False)

  # @param [Object]
  def default(self, o):
    if isinstance(o, MediaLogItem):
      return o.as_json()
    else:
      return super().default(self, o)

class AddNewItemsInterface:
  # @return [Array<MediaLogItem>]
  def get_new_items(self):
    try:
      while True:
        new_item = self.get_new_item()
        yield(new_item)

    except EOFError:
      return

  # @return [MediaLogItem]
  def get_new_item(self):
    title = input("Title? ")

    with self.completer(self.item_types_completer):
      type = input("Type? ")

    author = input("Author? ")

    with self.completer(self.current_date_completer):
      # In order to convert between formats (ISO to our own as defined by DATE_FORMAT_STRING), we
      # need to convert to a Datetime object and then back to string.
      date_completed_datetime = datetime.datetime.fromisoformat(input("Date Completed? "))
      date_completed_string = date_completed_datetime.strftime(DATE_FORMAT_STRING)

    return MediaLogItem(
      title=title,
      type=type,
      author=author,
      date_completed=date_completed_string
    )

  # @param [Function]
  @contextlib.contextmanager
  def completer(self, func):
    old_completer = readline.get_completer()
    readline.set_completer(func)
    yield
    readline.set_completer(old_completer)

  # These 'completer' functions follow the readline API. For each unique string the user types in,
  # they are called with state=0, with state=1, etc. Each result is displayed. They stop getting
  # called when they return a non-string value.

  # @param [String]
  # @param [Integer]
  # @return [String, None]
  @staticmethod
  def item_types_completer(text, state):
    possible_types = sorted(string for string in MediaLogItem.TYPES if string.startswith(text))
    return possible_types[state] if possible_types else None

  # @param [String]
  # @param [Integer]
  # @return [String, None]
  @staticmethod
  def current_date_completer(_text, state):
    current_date_formatted = datetime.date.today().isoformat()
    if state > 0: return None
    return current_date_formatted

class DataStore:
  FILENAME = "data/json/media_log.json"

  # @param [MediaLogItem]
  # @return [None]
  def write(self, item):
    media_log_items = self._read() + [item] # Put it on the end.
    sorted_media_log_items = sorted(media_log_items, key=lambda item: item.date_completed)

    self._write_all(sorted_media_log_items, MediaLogItemJsonEncoder())

  # @param [None]
  # @return [None]
  def print_last_few_items(self):
    few = 5
    print("Last {} items were:".format(few))
    items = data_store._read()
    for item in items[-few:]:
      print([item.title, item.date_completed.strftime(DATE_FORMAT_STRING)])
    print()

  # @param [Array<MediaLogItem>]
  # @param [json.JSONEncoder]
  # @return [None]
  def _write_all(self, media_log_items, encoder):
    json_representation = encoder.encode({ 'objects': media_log_items })
    
    with open(self.FILENAME, 'w') as file:
      print(json_representation, file=file)

  # @return [Array<MediaLogItem>]
  def _read(self):
    results = []
    with open(self.FILENAME) as file:
      media_log_json = json.loads(file.read())

      for json_object in media_log_json["objects"]:
        results.append(MediaLogItem(**json_object))

    return results

if __name__ == "__main__":
  readline.parse_and_bind('tab: complete')
  data_store = DataStore()

  # Help me remember what I logged already.
  data_store.print_last_few_items()

  print("Enter new items")
  for new_item in AddNewItemsInterface().get_new_items():
    data_store.write(new_item)

    print("***")
    print("Added '{}'.".format(new_item.pretty_printed()))
    print("***")
