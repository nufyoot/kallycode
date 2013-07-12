#include <cstdio>
#include <string>
#include <ctime>

// Used for finding memory leaks on windows.
#ifdef _DEBUG
#   define _CRTDBG_MAP_ALLOC
#   include <stdlib.h>
#   include <crtdbg.h>
#   define DEBUG_NEW new(_NORMAL_BLOCK, __FILE__, __LINE__)
#   define new DEBUG_NEW
#endif

using namespace std;

typedef wchar_t     Char;
typedef wstring     String;

template<class T> struct delete_pointer     { static inline void on(T  value) {               } };
template<class T> struct delete_pointer<T*> { static inline void on(T* value) { delete value; } };

enum TokenType : short
{
    Unknown         = 0,
    Type            = 1,
    Variable        = 2,
    Identifier      = 3,
    Operator        = 4,
    NumberLiteral   = 5,
    BinaryOperator  = 6
};

/***************************************************************************\
* Empty function signatures used by the structs.
\***************************************************************************/
bool        areEqual            (const String& first, const String& second);

/***************************************************************************\
* Represents a single token used for parsing a file.
\***************************************************************************/
struct Token 
{
    Token*      nextToken;
    TokenType   tokenType;

    Token(TokenType type) 
        : tokenType(type)
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
        : Token(TokenType::Type)
    {
        name = typeName;
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
    String* name;

    IdentifierToken(String* name, TokenType type)
        : Token(type)
    {
        this->name = name;
    }

    virtual ~IdentifierToken()
    {
        delete name;
    }
};

/***************************************************************************\
* Represents a variable declaration.
\***************************************************************************/
struct VariableDeclarationToken : IdentifierToken
{
    TypeToken*      type;

    VariableDeclarationToken(TypeToken* variableType, String* name)
        : IdentifierToken(name, TokenType::Variable)
    {
        type = variableType;
    }
};

/***************************************************************************\
* Represents an operator.
\***************************************************************************/
struct OperatorToken : Token
{
    String* value;

    OperatorToken(String* op)
        : Token(TokenType::Operator),
          value(op)
    { }

    virtual ~OperatorToken()
    {
        delete value;
    }
};

/***************************************************************************\
* Represents a number.
\***************************************************************************/
struct NumberLiteralToken : Token
{
    String* value;

    NumberLiteralToken(String* op)
        : Token(TokenType::NumberLiteral),
          value(op)
    { }

    virtual ~NumberLiteralToken()
    {
        delete value;
    }
};

/***************************************************************************\
* Represents a binary operator.
\***************************************************************************/
struct BinaryOperatorToken : Token
{
    Token*          left;
    Token*          right;
    OperatorToken*  binaryOperator;

    BinaryOperatorToken(Token* leftToken, Token* rightToken, OperatorToken* binaryOperatorToken)
        : Token(TokenType::BinaryOperator),
          left(leftToken),
          right(rightToken),
          binaryOperator(binaryOperatorToken)
    { }

    ~BinaryOperatorToken()
    {
        delete left;
        delete right;
        delete binaryOperator;
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
        if (value != nullptr) { delete_pointer<T>::on(value); }
    }
};

/***************************************************************************\
* Represents an iterator to iterate through a linked list.
\***************************************************************************/
template <typename T>
struct LinkedListIterator
{
private:
    LinkedListEntry<T>* startingMarker;

public:
    LinkedListEntry<T>* current;

    LinkedListIterator(LinkedListEntry<T>* first)
    { 
        current = startingMarker = new LinkedListEntry<T>(NULL);
        startingMarker->next = first;
    }

    ~LinkedListIterator()
    {
        delete startingMarker;
    }

    inline bool next()
    {
        return (current = current->next) != nullptr;
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
    short               count;

    LinkedList()
    {
        first = nullptr;
        last = nullptr;
        count = 0;
    }

    ~LinkedList()
    {
        clear(true);
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
        count++;
    }

    inline LinkedListIterator<T>* getIterator()
    {
        return new LinkedListIterator<T>(first);
    }

    void clear(bool disposeEntries)
    {
        while (first != nullptr)
        {
            LinkedListEntry<T>* next = first->next;
            if (!disposeEntries) { first->value = nullptr; }
            delete first;
            first = next;
        } 

        first = last = nullptr;
        count = 0;
    }
};

/***************************************************************************\
* Represents a scope to be used for determing what is available to code.
\***************************************************************************/
struct Scope
{
    LinkedList<TypeToken*>          types;
    LinkedList<IdentifierToken*>    identifiers;

    inline void addIdentifier(IdentifierToken* token)
    {
        identifiers.append(token);
    }

    inline void addType(TypeToken* typeToken)
    {
        types.append(typeToken);
    }

    bool findIdentifier(String* name, IdentifierToken** token)
    {
        LinkedListEntry<IdentifierToken*>* current = identifiers.first;
        while (current != nullptr)
        {
            if (areEqual(*current->value->name, *name))
            {
                *token = current->value;
                return true;
            }
            current = current->next;
        }

        return false;
    }

    bool findType(String* typeName, TypeToken** typeToken)
    {
        LinkedListEntry<TypeToken*>* current = types.first;
        while (current != nullptr)
        {
            if (areEqual(*current->value->name, *typeName))
            {
                *typeToken = current->value;
                return true;
            }
            current = current->next;
        }

        return false;
    }

    void init()
    {
        // This is just to get a basic scope with the core data.
        addType(new TypeToken(new String(L"int"), 4UL));
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
    const Char*         contents;
    unsigned int            position;
    Stack<unsigned int>*    markerStack;

    Tokenizer(const Char* input)
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

    inline Char current()
    {
        return contents[position];
    }
};

/***************************************************************************\
* Empty function signatures
\***************************************************************************/
bool        isOperatorChar              (Char token);
bool        nextStatementTokenList      (Tokenizer* tokenizer, LinkedList<Token*>** tokens);
bool        parse                       (const Char* fileName, Program** program);
bool        parseIdentifier             (Tokenizer* tokenizer, String** identifier);
bool        parseNextStatement          (Tokenizer* tokenizer, Scope* scope, Token** statement);
bool        parseNumber                 (Tokenizer* tokenizer, String** identifier);
bool        processBinaryOperatorToken  (Scope* scope, LinkedList<Token*>* tokens, OperatorToken* operatorToken, BinaryOperatorToken** token);
bool        processTokenListAsStatement (Scope* scope, LinkedList<Token*>* tokens, Token** statement);
void        skipWhitespace              (Tokenizer* tokenizer);
bool        isTokenAnIdentifier         (Token* token);

/***************************************************************************\
* Provides the main entry point for the program.
*
* Arguments:
*   argc - The number of arguments passed in to the program.
*   argv - The actual arguments passed in to the program.
*
* Returns:
*   Just returns 0, for now.
\***************************************************************************/
int main(int argc, const char** argv) 
{
    clock_t timer = clock();
    Program* program;

    const Char* input = 
        L"int i;"
        L"i = 5;";

    for (long i = 0; i < 1000000; i++) 
    {
        if (parse(input, &program))
        {
            delete program;
        }

#if _DEBUG
        break;
#endif
    }

    clock_t clocks = clock() - timer;
    printf("Finished in %2.4f seconds\n", (double)clocks / (double)CLOCKS_PER_SEC);

#if _DEBUG
    _CrtDumpMemoryLeaks();
#endif

    return 0;
}

/***************************************************************************\
* Determines if a character is a valid ASCII alpha character.
*
* Arguments:
*   c - The character to check
*
* Returns:
*   True if the character is [a-zA-z]
\***************************************************************************/
inline bool isAlpha(Char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

/***************************************************************************\
* Determines if a character is a valid digit.
*
* Arguments:
*   c - The character to check
*
* Returns:
*   True if the character is [0-9]
\***************************************************************************/
inline bool isDigit(Char c)
{
    return (c >= '0' && c <= '9');
}

/***************************************************************************\
* Parses some code and returns the program that represents the parsed code.
*
* Arguments:
*   [In ] contents - The contents of the code to be parsed.
*   [Out] program  - The resulting program that represents this code.
*
* Returns:
*   True if the statment was correctly parsed with no errors.
\***************************************************************************/
bool parse(const Char* contents, Program** program) 
{
    Program* result = new Program();
    Tokenizer tokenizer(contents);
    Token** currentToken = &result->firstToken;
    
    while (tokenizer.hasMore())
    {
        Token* newToken;
        if (parseNextStatement(&tokenizer, result->globalScope, &newToken))
        {
            if (isTokenAnIdentifier(newToken))
            {
                result->globalScope->addIdentifier((IdentifierToken*)newToken);
            }
            else
            {
                (*currentToken) = newToken;
                currentToken = &newToken->nextToken;
            }
        }
    }
    
    *program = result;
    return true;
}

/***************************************************************************\
* Parses the next statement from the tokenizer.
*
* Arguments:
*   [In ] tokenizer - This is the object helping tokenize the code.
*   [In ] scope     - This is the scope being used for the current statement.
*   [Out] statement - This is the resulting statement that was parsed.
*
* Returns:
*   True if the statment was correctly parsed with no errors.
\***************************************************************************/
bool parseNextStatement(Tokenizer* tokenizer, Scope* scope, Token** statement)
{
    bool result = false;
    
    LinkedList<Token*>* rawTokens;

    if (nextStatementTokenList(tokenizer, &rawTokens))
    {
        result = processTokenListAsStatement(scope, rawTokens, statement);

        if (result)
        {
            rawTokens->clear(false);
        }

        delete rawTokens;
    }

    return result;
}

/***************************************************************************\
* Processes the list of tokens as a single statement.
*
* Arguments:
*   [In ] scope     - This is the scope being used for the current statement.
*   [In ] tokens    - This is the list of tokens to be processed.
*   [Out] statement - This is the resulting statement that was processed.
*
* Returns:
*   True if the statment was correctly processed with no errors.
\***************************************************************************/
bool processTokenListAsStatement(Scope* scope, LinkedList<Token*>* tokens, Token** statement)
{
    bool result = false;

    if (tokens->count == 1)
    {
        *statement = tokens->first->value;
        tokens->clear(false);
        result = true;
    }
    else if (tokens->count == 2)
    {
        Token* first = tokens->first->value;
        Token* second = tokens->last->value;

        if (first->tokenType == TokenType::Identifier && second->tokenType == TokenType::Identifier)
        {
            // Very probable that this is a variable declaration.
            TypeToken* type;
            if (scope->findType(reinterpret_cast<IdentifierToken*>(first)->name, &type))
            {
                *statement = new VariableDeclarationToken(type, new String(reinterpret_cast<IdentifierToken*>(second)->name->c_str()));
                result = true;
            }
        }
        tokens->clear(true);
    }
    else {
        LinkedListIterator<Token*>* iterator = tokens->getIterator();
        while (iterator->next())
        {
            Token* token = iterator->current->value;
            TokenType tokenType = token->tokenType;

            if (tokenType == TokenType::Operator) 
            {
                OperatorToken* operatorToken = reinterpret_cast<OperatorToken*>(token);
            
                if (areEqual(*operatorToken->value, String(L"=")))
                {
                    result = processBinaryOperatorToken(scope, tokens, operatorToken, reinterpret_cast<BinaryOperatorToken**>(statement));
                    break;
                }
            }
        }
        delete iterator;
    }

    return result;
}

/***************************************************************************\
* Processes the list of tokens as a binary operator token.
*
* Arguments:
*   [In ] scope         - This is the scope being used for the current 
*                         statement.
*   [In ] tokens        - This is the list of tokens that make up the binary 
*                         operator.
*   [In ] operatorToken - This is the operator for the token.
*   [Out] statement     - This is the resulting statement that was processed.
*
* Returns:
*   True if the statment was correctly processed with no errors.
\***************************************************************************/
bool processBinaryOperatorToken(Scope* scope, LinkedList<Token*>* tokens, OperatorToken* operatorToken, BinaryOperatorToken** token)
{
    LinkedList<Token*> left;
    LinkedList<Token*> right;
    LinkedListIterator<Token*>* iterator = tokens->getIterator();
    bool result = true;

    while (iterator->next() && iterator->current->value != operatorToken)
    {
        left.append(iterator->current->value);
    }

    while (iterator->next())
    {
        right.append(iterator->current->value);
    }

    Token* leftToken = nullptr;
    Token* rightToken = nullptr;

    if (!processTokenListAsStatement(scope, &left, &leftToken) ||
        !processTokenListAsStatement(scope, &right, &rightToken))
    {
        result = false;
        left.clear(false);
        right.clear(false);
    }

    if (result)
    {
        tokens->clear(false);
        result = true;
        *token = new BinaryOperatorToken(leftToken, rightToken, operatorToken);
    }
    
    delete iterator;
    return result;
}

/***************************************************************************\
* Gets the next token in its simplest form.
*
* Arguments:
*   [In ] tokenizer - The tokenizer that is pulling characters from the code.
*   [Out] result    - This is the resulting token that comes next in the code.
*
* Returns:
*   True if there was a token to be pulled.
\***************************************************************************/
bool nextToken(Tokenizer* tokenizer, Token** result)
{
    skipWhitespace(tokenizer);

    if (isOperatorChar(tokenizer->current()))
    {
        *result = new OperatorToken(new String(1, tokenizer->current()));
        tokenizer->next();
        return true;
    }
    else if (isDigit(tokenizer->current()))
    {
        String* rawToken;
        parseNumber(tokenizer, &rawToken);
        *result = new NumberLiteralToken(rawToken);
        return true;
    }
    else 
    {
        String* rawToken;
        parseIdentifier(tokenizer, &rawToken);
        *result = new IdentifierToken(rawToken, TokenType::Identifier);
        return true;
    }

    return false;
}

/***************************************************************************\
* Gets the next statement in code and passes back the list of tokens that
* makes up the statement.
*
* Arguments:
*   [In ] tokenizer - The tokenizer that is pulling characters from the code.
*   [Out] tokens    - This is the resulting tokens that make up the statement.
*
* Returns:
*   True if the statment was correctly parsed with no errors.
\***************************************************************************/
bool nextStatementTokenList(Tokenizer* tokenizer, LinkedList<Token*>** tokens)
{
    LinkedList<Token*>* list = new LinkedList<Token*>();
    bool result = false;

    while (tokenizer->hasMore() && !result)
    {
        Token* rawToken;
        if (!nextToken(tokenizer, &rawToken))
        {
            break;
        }

        if (rawToken->tokenType == TokenType::Operator)
        {
            OperatorToken* operatorToken = reinterpret_cast<OperatorToken*>(rawToken);
            if (areEqual(*operatorToken->value, String(L";")))
            {
                result = true;
                delete rawToken;
            }
        }

        if (!result)
        {
            list->append(rawToken);
        }
    }

    if (result) { *tokens = list; }
    else        { delete list;    }

    return result;
}

/***************************************************************************\
* Pulls an identifier from the code.
*
* Arguments:
*   [In ] tokenizer     - This is the object helping tokenize the code.
*   [Out] identifier    - This is the resulting identifier.
*
* Returns:
*   True if the identifier was correctly pulled.
\***************************************************************************/
bool parseIdentifier(Tokenizer* tokenizer, String** identifier)
{
    skipWhitespace(tokenizer);

    unsigned int start = tokenizer->position;
    while (isAlpha(tokenizer->current()) && tokenizer->hasMore())
    {
        tokenizer->next();
    }
    unsigned int finish = tokenizer->position;
    unsigned int length = finish - start;

    *identifier = new String(tokenizer->contents + start, length);
    return true;
}

/***************************************************************************\
* Pulls a number from the code.
*
* Arguments:
*   [In ] tokenizer     - This is the object helping tokenize the code.
*   [Out] identifier    - This is the resulting number.
*
* Returns:
*   True if the number was correctly pulled.
\***************************************************************************/
bool parseNumber(Tokenizer* tokenizer, String** identifier)
{
    skipWhitespace(tokenizer);

    unsigned int start = tokenizer->position;
    while (isDigit(tokenizer->current()) && tokenizer->hasMore())
    {
        tokenizer->next();
    }
    unsigned int finish = tokenizer->position;
    unsigned int length = finish - start;

    *identifier = new String(tokenizer->contents + start, length);
    return true;
}

/***************************************************************************\
* Skips the whitespace found in the code.
*
* Arguments:
*   tokenizer   - This is the object helping tokenize the code.
\***************************************************************************/
inline void skipWhitespace(Tokenizer* tokenizer)
{
    while (isspace(tokenizer->current()) && tokenizer->hasMore())
    {
        tokenizer->next();
    }
}

/***************************************************************************\
* Determines if this is an identifier token.
*
* Arguments:
*   token   - This is the token to test against.
*
* Returns:
*   True if the token is some form of an identifier token.
\***************************************************************************/
inline bool isTokenAnIdentifier(Token* token)
{
    return token->tokenType == TokenType::Variable;
}

/***************************************************************************\
* Determines if this is an operator character.
*
* Arguments:
*   c   - This is the character to test against.
*
* Returns:
*   True if the token is some form of an operator character.
\***************************************************************************/
inline bool isOperatorChar(Char c)
{
    return c == '='
        || c == ';';
}

/***************************************************************************\
* Determines if two strings are equal.
*
* Arguments:
*   first   - The first string to test.
*   second  - The second string to test against.
*
* Returns:
*   True if the strings are equal.
\***************************************************************************/
inline bool areEqual(const String& first, const String& second)
{
    return first.compare(second) == 0;
}