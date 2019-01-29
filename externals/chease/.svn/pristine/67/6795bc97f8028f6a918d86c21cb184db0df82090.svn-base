/*--------------------------------------------------------------------
 * C helper routines of xml2eg
 *   Edmondo Giovannozzi (ENEA-EUROFUSION 2010)
 *                                                                    */

#include <stdio.h>
#include <string.h>

#include <libxml/parser.h>
#include <libxml/tree.h>

int libxml2_f_isDocumentEmpty(const xmlDocPtr docm) {
    if (docm == NULL) {
        return 1;
    } else {
        return 0;
    }
}
int libxml2_f_isNodeEmpty(const xmlNodePtr docm) {
    if (docm == NULL) {
        return 1;
    } else {
        return 0;
    }
}

xmlNodePtr libxml2_f_findNodeByName(const xmlNodePtr node, const char *name, int l_name) {
    xmlNode *cur_node = NULL;
    xmlNodePtr child_node = NULL;
    xmlNodePtr return_node = NULL;
    int i;
    if (node == NULL) {
        return NULL;
    }
    child_node = node->children;
    for (cur_node = child_node; cur_node; cur_node = cur_node->next) {
        if (cur_node->type == XML_ELEMENT_NODE) {
/*            printf("node type: Element, name: %s\n", cur_node->name);
            printf("-- %d\n", strncmp(name, cur_node->name, l_name)); */
            if (strncmp(name, cur_node->name, l_name)==0) {
                return_node = cur_node;
                break;
            }
        }
    }

    return return_node;
}

void libxml2_f_NodeGetContent(const xmlNodePtr node, char *content, size_t l_content) {
    xmlChar * cont;
    int i;
    cont = xmlNodeGetContent(node);
    
/*  char *strncpy(char *dest, const char *src, size_t n); */   
    strncpy(content, cont, l_content);
    xmlFree(cont);

/* change new line and tabs to space */
   for(i=0;i<l_content; i++) {
        if ( content[i] == '\t' | content[i] == '\n'  | content[i] == '\r')  content[i] = ' ';
    }
}

void libxml2_f_NodeGetContentRaw(const xmlNodePtr node, xmlChar **cont, size_t *l_content) {
    /*I need to return the pointer so I declare it a pointer to a pointer */
    *cont = xmlNodeGetContent(node);
    *l_content = xmlStrlen(*cont);
}

void libxml2_f_ContentCopy(xmlChar *cont, char* cstr, size_t l_str) {
    size_t i;
    
    strncpy(cstr, cont, l_str);

/*  I cannot declare it as "const xmlChar * cont" as I have to deallocate it */    

    xmlFree(cont);

/* change new line and tabs to space */
   for(i=0;i<l_str; i++) {
        if ( cstr[i] == '\t' | cstr[i] == '\n'  | cstr[i] == '\r')  cstr[i] = ' ';
    }
}

//void getnodename_(const xmlNodePtr element, char *name, int l_name) {

///*  char *strncpy(char *dest, const char *src, size_t n); */
    //strncpy(name, element->name, l_name);

//}
