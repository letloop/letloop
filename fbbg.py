from pathlib import Path
from subprocess import run
from lxml.html import fromstring as string2html
from lxml.html import tostring as html2string
import shlex
from datetime import datetime
from feedgen.feed import FeedGenerator
import pytz


def render(root, path):
    print('** reading {}'.format(path))
    directory = path.parent
    command = 'pandoc --from=markdown+yaml_metadata_block+inline_notes --mathml --standalone {} --template={} --output={}'
    filename = '.'.join(path.name.split('.')[:-1]) + '.html'
    command = command.format(path, root / 'template.html', directory / filename)
    run(shlex.split(command))

def read(root, path):
    print('** reading {}'.format(path))
    with path.open('r') as f:
        html = f.read()
    html = string2html(html)

    try:
        title = html.xpath("//meta[@key='title']/@value")[0]
        date = html.xpath("//meta[@key='date']/@value")[0]
        abstract = html.xpath("//meta[@key='abstract']/@value")[0]
        date = datetime.fromisoformat(date)
        date = date.replace(tzinfo=pytz.UTC)
        body = html2string(html.xpath("//div[@id='root']")[0])
        out = str(path.parent)[len(str(root)) + 1:] + '/'
        return out, title, date, abstract, body
    except Exception as exc:
        print("Ignoring '{}' failed to gather metadata because of the following error: {}".format(path, exc))
        now = datetime.now()
        now = now.replace(tzinfo=pytz.UTC)
        return path, None, now, None, None

def make(directory):
    print('* converting md to html')
    root = Path('.').resolve()
    target = root / directory
    for path in target.rglob('*/*/index.md'):
        if str(path).startswith('.'):
            continue
        path = path.resolve()
        render(root, path)
    print('* gathering metadata')
    meta = []
    for path in target.rglob('*/*/index.html'):
        if str(path).startswith('.'):
            continue
        if str(path) == 'index.html':
            continue
        meta.append(read(root, path))
    meta.sort(key=lambda x: (x[2], x[1]), reverse=True)

    with open('header.md') as f:
        out = f.read()

    for path, title, date, abstract, _ in meta:
        out += "\n\n## [{}](/{})\n\n\n{}: {} \n\n".format(title, path, date.strftime("%Y-%m-%d"), abstract)

    print('* Creating index')

    with (target / 'index.md').open('w') as f:
        f.write(out)
    render(root, (target / 'index.md').resolve())
    print('* Creating feeds')

    fg = FeedGenerator()
    fg.id('https://hyper.dev')
    fg.title('hyper.dev')

    fg.link( href='https://hyper.dev', rel='alternate' )
    fg.subtitle('The easy, fine & neat geek from another world!')
    fg.language('en')

    for item in meta:
        path, title, date, abstract, body = item
        if not (path and title and date and abstract and body):
            print("Ooops with {}".format(item))
            continue
        fe = fg.add_entry()
        url = 'https://hyper.dev/{}'.format(path)
        fe.id(url)
        fe.guid(url, permalink=True)
        fe.title(title)
        fe.link(href=url)
        fe.summary(abstract)
        fe.description(abstract)
        fe.published(date)
        fe.content(body, type='html')
    fg.atom_file(str(target / 'atom.xml'))
    fg.rss_file(str(target / 'feed.xml'))


def main():
    make('log')
    make('lib')


if __name__ == '__main__':
    main()
