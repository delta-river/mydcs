import sys
import spacy

nlp = spacy.load("en_core_web_sm")

def children(doc, parent):
    return [token for token in doc if (token.head == parent and token != parent)]
    
#tree syntax is as below
#({root.text|root.lemma};\tchild1\tchild2....)
def node(doc, token):
    node_info = "{" + token.text.lower() + "|" + token.lemma_.lower() + "}"
    children_info = ""
    for child in children(doc, token):
        children_info = children_info + "\t" + node(doc, child)
    return "(" + node_info + ";" + children_info + ")"

def query_process(query, f):
    doc = nlp(query)
    #for token in doc:
        #print(token.text, token.lemma_, token.head.text)
    #ignore those not included in the root tree
    root = [token for token in doc if token.head == token][0]
    tree = node(doc, root) + "\n"
    f.write(tree)

if (len(sys.argv) > 2):

    #input file
    input_path = sys.argv[1]
    #output file
    output_path = sys.argv[2] + ".tree"

    #input queries
    with open(input_path) as f:
        queries = f.readlines()

    #output trees
    with open(output_path, mode='w') as f:
        for raw_query in queries:
            query = raw_query.replace('\n', '')
            if (query != "") :
                query_process(query.replace('\n', ''), f)
else:
    print("!!!!!!!!!!!!!!!!!too feww arguments: QueryProcessor!!!!!!!!!!!!!!!!!!!!!!!!")
