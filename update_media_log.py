import json
import inflection # Provides camelCase --> snake_case conversion.
# Not used in the code, but this line causes it to be implicitly used by input(). It provides better
# line-editing and history functionality.
import readline

class MediaLogItem:
  ALBUM = 'album'
  NOVEL = 'novel'
  NONFICTION = 'nonfiction'
  SHORT_STORIES = 'short stories'
  MOVIE = 'movie'
  TV_SHOW = 'tv show'
  POETRY = 'poetry'
  DOCUMENTARY = 'documentary'
  WEBCOMIC = 'webcomic'
  COMIC = 'comic'

  TYPES = [
    ALBUM,
    NOVEL,
    NONFICTION,
    SHORT_STORIES,
    MOVIE,
    TV_SHOW,
    POETRY,
    DOCUMENTARY,
    WEBCOMIC,
    COMIC,
  ]

  def __init__(self, *, title, type, author, date_completed):
    if type not in MediaLogItem.TYPES:
      raise ValueError("{} is not recognized.".format(type))

    self.title = title
    self.type = type
    self.author = author
    self.date_completed = date_completed

  # @return [Dictionary]
  def as_json(self):
    return {
      'title': self.title,
      'type': self.type,
      'author': self.author,
      'dateCompleted': self.dateCompleted,
    }

class MediaLogItemJsonEncoder(json.JSONEncoder):
  def __init__(self):
    super().__init__(self, indent=4, sort_keys=True, ensure_ascii=False)

  # @param [Object]
  def default(self, o):
    if isinstance(o, MediaLogItem):
      return o.as_json()
    else:
      return super().default(self, o)

# @param [Array<MediaLogItem>]
# @param [String]
def write_back(media_log_items, destination_filename):
  encoder = MediaLogItemJsonEncoder()
  json_representation = encoder.encode({ 'objects': media_log_items })
  
  with open(destination_filename, 'w') as file:
    print(json_representation, file=file)

# @return [MediaLogItem]
def accept_new_item_from_user():
  title = input("Title? ")
  type = input("Type? ")
  author = input("Author? ")
  date_completed = input("Date Completed? ")

  return MediaLogItem(
    title=title,
    type=type,
    author=author,
    date_completed=date_completed
  )

if __name__ == "__main__":
  source_filename = "data/json/media_log.json"
  with open(source_filename) as file:
    media_log_json = json.loads(file.read())

    media_log_items = []
    for json_object in media_log_json["objects"]:
      snake_cased_json_object = {
        inflection.underscore(key): value for key, value in json_object.items()
      }
      media_log_items.append(MediaLogItem(**snake_cased_json_object))

  accept_new_item_from_user()
  # write_back(media_log_items, "data/json/media_log_2.json")
