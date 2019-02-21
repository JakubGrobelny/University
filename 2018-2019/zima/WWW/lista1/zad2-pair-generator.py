tags = [
    'article', 
    'aside', 
    'header', 
    'footer', 
    'address', 
    'section', 
    'nav', 
    'h1', 
    'h2', 
    'a'
]

def tag_opening(tag : str) -> str:
    return '<' + tag + '>'

def tag_closing(tag : str) -> str:
    return '</' + tag + '>'

def main():
    for tag1 in tags:
        for tag2 in tags:
            print(tag_opening(tag1) + tag_opening(tag2) 
                + tag_closing(tag2) + tag_closing(tag1))

main()