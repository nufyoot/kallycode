#include <stdio.h>
#include <fstream>
#include <cstring>
#include <type_traits>

using namespace std;

#define TO_CHAR16(a) reinterpret_cast<char16_t*>(a)

typedef basic_string<char16_t>          String;

template<class T> struct delete_pointer     { static inline void on(T  value) {               } };
template<class T> struct delete_pointer<T*> { static inline void on(T* value) { delete value; } };

/***************************************************************************\
* Represents a single token used for parsing a file.
\***************************************************************************/
struct Token 
{
    Token* nextToken;

    Token()
    {
        nextToken = nullptr;
    }

    virtual ~Token() 
    {
        if (nextToken != nullptr) { delete nextToken; }
    }
};

/***************************************************************************\
* Represents a type.
\***************************************************************************/
struct TypeToken : Token
{
    unsigned long   typeSizeInMemory;
    String*         name;

    TypeToken(String* typeName, unsigned long sizeInMemory = 0)
        : Token()
    {
        name = new String(typeName->c_str());
        typeSizeInMemory = sizeInMemory;
    }

    TypeToken(char16_t* typeName, unsigned long sizeInMemory = 0)
        : Token()
    {
        name = new String(typeName);
        typeSizeInMemory = sizeInMemory;
    }

    virtual ~TypeToken()
    {
        delete name;
    }
};

/***************************************************************************\
* Represents an identifier, such as a function, variable or constant.
\***************************************************************************/
struct IdentifierToken : Token
{
    String*         name;

    IdentifierToken(String* name)
        : Token()
    {
        this->name = new String(name->c_str());
    }

    IdentifierToken(char16_t* name)
        : Token()
    {
        this->name = new String(name);
    }

    virtual ~IdentifierToken()
    {
        delete name;
    }
};

/***************************************************************************\
* Represents a variable.
\***************************************************************************/
struct VariableToken : IdentifierToken
{
    TypeToken*      type;

    VariableToken(TypeToken* variableType, String* typeName)
        : IdentifierToken(typeName)
    {
        type = variableType;
    }

    VariableToken(TypeToken* variableType, char16_t* typeName)
        : IdentifierToken(typeName)
    {
        type = variableType;
    }
};

/***************************************************************************\
* Represents a single entry in a stack
\***************************************************************************/
template <typename T> 
struct StackEntry
{
    StackEntry* next;
    T           value;

    StackEntry() 
    {
        next = nullptr;
    }

    virtual ~StackEntry()
    {
        delete_pointer<T>::on(value);
    }
};

/***************************************************************************\
* Represents a nice, light weight stack
\***************************************************************************/
template <typename T> 
struct Stack
{
    Stack()
    {
        first = nullptr;
    }

    ~Stack()
    {
        while (first != nullptr)
        {
            StackEntry<T>* next = first->next;
            delete first;
            first = next;
        }
    }

    void push(T element)
    {
        StackEntry<T>* newEntry = new StackEntry<T>();
        newEntry->value = element;
        newEntry->next = first;
        first = newEntry;
    }

    bool pop(T* result)
    {
        if (first == nullptr)
        {
            return false;
        }

        *result = first->value;
        first = first->next;
        return true;
    }

    StackEntry<T>* first;
};

/***************************************************************************\
* Represents a nice, light weight linked list entry.
\***************************************************************************/
template <typename T>
struct LinkedListEntry
{
    LinkedListEntry<T>* next;
    T                   value;

    LinkedListEntry(T value)
    {
        this->next = nullptr;
        this->value = value;
    }

    virtual ~LinkedListEntry()
    {
        delete_pointer<T>::on(value);
    }
};

/***************************************************************************\
* Represents a nice, light weight linked list.
\***************************************************************************/
template <typename T>
struct LinkedList
{
    LinkedListEntry<T>* first;
    LinkedListEntry<T>* last;

    LinkedList()
    {
        first = nullptr;
        last = nullptr;
    }

    ~LinkedList()
    {
        while (first != nullptr)
        {
            LinkedListEntry<T>* next = first->next;
            delete first;
            first = next;
        }
    }

    void append(T entry)
    {
        if (first == nullptr)
        {
            first = last = new LinkedListEntry<T>(entry);
        }
        else
        {
            LinkedListEntry<T>* newEntry = new LinkedListEntry<T>(entry);
            last->next = newEntry;
            last = newEntry;
        }
    }
};

/***************************************************************************\
* Represents a scope to be used for determing what is available to code.
\***************************************************************************/
struct Scope
{
    LinkedList<TypeToken*>  types;

    void addType(TypeToken* typeToken)
    {
        types.append(typeToken);
    }

    bool findType(String* typeName, TypeToken** typeToken)
    {
        LinkedListEntry<TypeToken*>* token = types.first;
        while (token != nullptr)
        {
            if (token->value->name->compare(*typeName) == 0)
            {
                *typeToken = token->value;
                return true;
            }
            token = token->next;
        }

        return false;
    }

    void init()
    {
        // This is just to get a basic scope with the core data.
        addType(new TypeToken(TO_CHAR16(L"int"), 4UL));
    }
};

/***************************************************************************\
* Represents a program.
\***************************************************************************/
struct Program
{
    Scope*  globalScope;
    Token*  firstToken;

    Program()
    {
        globalScope = new Scope();
        globalScope->init();
        firstToken = nullptr;
    }

    ~Program()
    {
        if (globalScope) { delete globalScope; }
        if (firstToken)  { delete firstToken;  }
    }
};

/***************************************************************************\
* Represents a single struct used for tokenizing code.
\***************************************************************************/
struct Tokenizer
{
    const char16_t*         contents;
    unsigned int            position;
    Stack<unsigned int>*    markerStack;

    Tokenizer(const char16_t* input)
    {
        contents = input;
        position = 0;
        markerStack = new Stack<unsigned int>();
    }

    virtual ~Tokenizer()
    {
        delete markerStack;
    }

    inline unsigned int createMarker()
    {
        markerStack->push(position);
        return position;
    }

    inline unsigned int restoreLastMarker()
    {
        markerStack->pop(&position);
        return position;
    }

    inline bool hasMore()
    {
        return contents[position] != '\0';
    }

    inline bool next()
    {
        return contents[++position] != '\0';
    }

    inline char16_t current()
    {
        return contents[position];
    }
};

/***************************************************************************\
* Empty function signatures
\***************************************************************************/
bool        parse               (const char16_t* fileName, Program** program);
bool        parseIdentifier     (Tokenizer* tokenizer, Scope* scope, String** identifier);
bool        parseNextStatement  (Tokenizer* tokenizer, Scope* scope, Token** statement);
void        skipWhitespace      (Tokenizer* tokenizer);

/***************************************************************************\
* Provides the main entry point for the program.
*
* Arguments:
*   argc - The number of arguments passed in to the program.
*   argv - The actual arguments passed in to the program.
\***************************************************************************/
int main(int argc, const char** argv) 
{
    Program* program;
    char16_t* input;

    input = TO_CHAR16(
        L"int i;"
        L"i = 5;"
    );

    for (long i = 0; i < 10000000; i++) 
    {
        if (parse(input, &program))
        {
            delete program;
        }
    }

    return 0;
}

/***************************************************************************\
* Determines if a character is a valid ASCII alpha character.
*
* Arguments:
*   c - The character to check
\***************************************************************************/
bool isAlpha(char16_t c)
{
    return (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z');
}

/***************************************************************************\
* Parses some code and returns the first token that then points to all
* subsequent tokens that make up the provided code.
*
* Arguments:
*   fileName - The file name of the file to parse.
\***************************************************************************/
bool parse(const char16_t* contents, Program** program) 
{
    Program* result = new Program();
    Tokenizer tokenizer(contents);
    Token* currentToken;

    parseNextStatement(&tokenizer, result->globalScope, &currentToken);
    result->firstToken = currentToken;
    
    while (tokenizer.hasMore())
    {
        Token* newToken;
        if (parseNextStatement(&tokenizer, result->globalScope, &newToken))
        {
            currentToken->nextToken = newToken;
            currentToken = newToken;
        }
    }
    
    *program = result;
    return true;
}

/***************************************************************************\
* Parses the next statement from the tokenizer.
*
* Arguments:
*   tokenizer - This is the object helping tokenize the code.
\***************************************************************************/
bool parseNextStatement(Tokenizer* tokenizer, Scope* scope, Token** statement)
{
    skipWhitespace(tokenizer);

    unsigned int start = tokenizer->position;
    while (!isspace(tokenizer->current()) && tokenizer->hasMore())
    {
        tokenizer->next();
    }
    unsigned int finish = tokenizer->position;
    unsigned int length = finish - start;
    String rawToken(tokenizer->contents + (start * sizeof(char16_t)), length);
    
    TypeToken* type;
    bool result = false;

    if (scope->findType(&rawToken, &type))
    {
        // Could be a function/variable declaration.
        String* identifier;
        if (parseIdentifier(tokenizer, scope, &identifier))
        {
            skipWhitespace(tokenizer);
            
            if (tokenizer->current() == ';')
            {
                tokenizer->next();
                *statement = new VariableToken(type, identifier);
                result = true;
            }

            delete identifier;
        }
    }

    return result;
}

/***************************************************************************\
* Parses an identifier.
*
* Arguments:
*   tokenizer - This is the object helping tokenize the code.
\***************************************************************************/
bool parseIdentifier(Tokenizer* tokenizer, Scope* scope, String** identifier)
{
    skipWhitespace(tokenizer);

    unsigned int start = tokenizer->position;
    while (isAlpha(tokenizer->current()) && tokenizer->hasMore())
    {
        tokenizer->next();
    }
    unsigned int finish = tokenizer->position;
    unsigned int length = finish - start;

    *identifier = new String(tokenizer->contents + (start * sizeof(char16_t)), length);
    return true;
}

/***************************************************************************\
* Skips all white space in the tokenizer.
*
* Arguments:
*   tokenizer - This is the object helping tokenize the code.
\***************************************************************************/
void skipWhitespace(Tokenizer* tokenizer)
{
    while (isspace(tokenizer->current()) && tokenizer->hasMore())
    {
        tokenizer->next();
    }
}