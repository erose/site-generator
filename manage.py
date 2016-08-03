#!/usr/bin/env python

import os, sys, glob, shutil, time, subprocess
from os.path import join, getmtime
import util
import markdown, jinja2

OUTPUT_DIR = 'dev_site'
if __name__ == "__main__":
    if sys.argv[1] == "publish":
        OUTPUT_DIR = 'rendered_site'

TEMPLATES_DIR = 'templates'
PAGE_NAMES = [
    "about.md", "turing.md", "fraction_pattern.md", "markov_text.md",
    "password_hasher.md"
]
PAGES_IN_DIR = '.'
PAGES_OUT_DIR = OUTPUT_DIR
POSTS_IN_DIR = 'posts'
POSTS_OUT_DIR = OUTPUT_DIR + '/posts'
STATIC_IN_DIR = 'static'
STATIC_OUT_DIR = OUTPUT_DIR + '/static'

# Tell Jinja where to load templates.
from jinja2 import Environment, FileSystemLoader
JINJA_ENV = Environment(loader=FileSystemLoader(TEMPLATES_DIR))

def render_posts():
    """ Writes out markdown posts via template into the output directory. """
    os.makedirs(POSTS_OUT_DIR, exist_ok=True)
    util.clean_folder(POSTS_OUT_DIR)

    for name in os.listdir(POSTS_IN_DIR):
        render_post(name)

def render_post(name):
    template = JINJA_ENV.get_template('post.html')

    # Parse the post.md into a dict.
    with open(join(POSTS_IN_DIR, name), "r", encoding="utf-8") as f:
        data = util.parse_md(f)

    # Hydrate the template and write it out.
    out_filename = join(POSTS_OUT_DIR, name).replace(".md", ".html")
    with open(out_filename, "w", encoding="utf-8") as f:
        f.write(template.render(**data))

def render_main():
    template = JINJA_ENV.get_template('index.html')

    posts = []
    for name in os.listdir(POSTS_IN_DIR):
        # Parse the post.md into a dict.
        with open(join(POSTS_IN_DIR, name), "r", encoding="utf-8") as f:
            data = util.parse_md(f)

        if data.get('status') == 'unfinished':
            continue

        # Add some extra metadata for convenience.
        data['url'] = "/posts/{}".format(
            data["name"].replace(".md", ".html")
        )
        # Truncate content to provide a preview.
        data['content'] = util.first_n_tags(data['content'], 3)

        posts.append(data)

    with open(OUTPUT_DIR + '/index.html', "w", encoding="utf-8") as f:
        f.write(template.render(posts=posts))

def render_pages():
    """ Writes out pages into the output directory. """
    util.clean_folder(PAGES_OUT_DIR, "*.html")

    for name in glob.glob(join(PAGES_IN_DIR, '*.md')):
        if os.path.basename(name) not in PAGE_NAMES:
            print("Ignoring {}.".format(name))
            continue

        render_page(name)

def render_page(name):
    template = JINJA_ENV.get_template('post.html')
    data = util.parse_md(open(name, "r", encoding="utf-8"))

    # Hydrate the template with the page.md dict and write it out.
    out_filename = join(PAGES_OUT_DIR, name).replace(".md", ".html")
    with open(out_filename, "w", encoding="utf-8") as f:
        f.write(
            template.render(
                content=data["content"],
                title=data["title"]
            )
        )

def static_files():
    for dirpath, _, filenames in os.walk(STATIC_IN_DIR):
        for filename in filenames:
            full_path = join(dirpath, filename)
            yield full_path

def render_static():
    """ Copy static files to the output directory. """
    shutil.rmtree(join(OUTPUT_DIR, 'static'), ignore_errors=True)
    for filename in static_files():
        render_static_file(filename)

def render_static_file(name):
    os.makedirs(join(OUTPUT_DIR, os.path.dirname(name)), exist_ok=True)
    shutil.copy(name, join(OUTPUT_DIR, name))

def render():
    render_static()
    render_pages()
    render_posts()
    render_main()

def serve():
    server_proc = subprocess.Popen(
        ["python", "-m", "http.server", "8000"], cwd=OUTPUT_DIR
    )

    POSTS = glob.glob(join(POSTS_IN_DIR, "*"))
    post_statuses = {filename: time.time() for filename in POSTS}
    PAGES = glob.glob(join(PAGES_IN_DIR, '*.md'))
    page_statuses = {filename: time.time() for filename in PAGES}
    STATIC_FILES = list(static_files())
    static_file_statuses = {
        filename: time.time() for filename in STATIC_FILES
    }
    TEMPLATES = glob.glob(join(TEMPLATES_DIR, "*"))
    template_statuses = {filename: time.time() for filename in TEMPLATES}

    try:
        while True:
            for filename, last_updated in post_statuses.items():
                if getmtime(filename) > last_updated:
                    print("Detected change to {}. Re-rendering."
                        .format(filename)
                    )
                    post_statuses[filename] = time.time()
                    # Rendering posts accepts only unqualified names.
                    render_post(os.path.basename(filename))
                    # Main may have been affected too.
                    render_main()
                    print("Re-rendering complete.")

            for filename, last_updated in page_statuses.items():
                if getmtime(filename) > last_updated:
                    print("Detected change to {}. Re-rendering."
                        .format(filename)
                    )
                    page_statuses[filename] = time.time()
                    render_page(filename)
                    print("Re-rendering complete.")

            for filename, last_updated in static_file_statuses.items():
                if getmtime(filename) > last_updated:
                    print("Detected change to {}. Re-rendering."
                        .format(filename)
                    )
                    static_file_statuses[filename] = time.time()
                    render_static_file(filename)
                    print("Re-rendering complete.")

            for filename, last_updated in template_statuses.items():
                if getmtime(filename) > last_updated:
                    print("Detected change to {}. Re-rendering."
                        .format(filename)
                    )
                    template_statuses[filename] = time.time()
                    render()
                    print("Re-rendering complete.")
                
            time.sleep(1)
    except (KeyboardInterrupt, Exception):
        server_proc.kill()
        raise

if __name__ == "__main__":
    if sys.argv[1] == "render":
        render()
    elif sys.argv[1] == "publish":
        render()
    elif sys.argv[1] == "serve":
        serve()
    elif sys.argv[1] == "all":
        render()
        serve()