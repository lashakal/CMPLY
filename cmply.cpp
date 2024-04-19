#include <iostream>
#include <vector>
#include <string>
#include <cctype>
#include <sstream>
#include <unordered_map>
#include <stdexcept>
#include <fstream>

enum class TokenType {
    Integer, Float, Character, String, Identifier, Keyword, Operator, EndOfFile
};

struct Token {
    TokenType type;
    std::string value;
};

class Lexer {
public:
    Lexer(const std::string& source) : source(source), position(0) {}

    std::vector<Token> tokenize() {
        std::vector<Token> tokens;
        while (position < source.length()) {
            if (std::isspace(source[position])) {
                position++;
            } else if (std::isdigit(source[position])) {
                tokens.push_back(readNumber());
            } else if (source[position] == '\'') {
                tokens.push_back({TokenType::Character, readCharacter()});
            } else if (source[position] == '"') {
                tokens.push_back({TokenType::String, readString()});
            } else if (std::isalpha(source[position])) {
                std::string identifier = readIdentifier();
                if (isKeyword(identifier)) {
                    tokens.push_back({TokenType::Keyword, identifier});
                } else {
                    tokens.push_back({TokenType::Identifier, identifier});
                }
            } else if (source[position] == ';' || source[position] == '(' || source[position] == ')' ||
                    source[position] == '{' || source[position] == '}') {
                tokens.push_back({TokenType::Operator, std::string(1, source[position++])});
            } else if (source[position] == '+' || source[position] == '-' || source[position] == '*' ||
                    source[position] == '/' || source[position] == '<' || source[position] == '>' ||
                    source[position] == '=' || source[position] == '!') {
                tokens.push_back(readOperator());
            } else {
                throw std::runtime_error("Unexpected character in input: " + std::string(1, source[position]));
            }
        }
        tokens.push_back({TokenType::EndOfFile, ""});
        return tokens;
    }

private:
    std::string source;
    size_t position;

    Token readNumber() {
        size_t start = position;
        bool isFloat = false;
        while (position < source.length() && (std::isdigit(source[position]) || source[position] == '.')) {
            if (source[position] == '.') {
                if (isFloat) throw std::runtime_error("Multiple decimal points in number");
                isFloat = true;
            }
            position++;
        }
        std::string value = source.substr(start, position - start);
        return {isFloat ? TokenType::Float : TokenType::Integer, value};
    }

    std::string readCharacter() {
        position++;  // skip opening quote
        size_t start = position;
//        if (position < source.length() && source[position] == '\\') {
//            position++; // escape character
//        }
        while (position < source.length() && source[position] != '\'') position++;
        position++;  // skip closing quote
        return source.substr(start, position - start - 1);
    }

    std::string readString() {
        position++;  // skip opening quote
        size_t start = position;
        while (position < source.length() && source[position] != '"') position++;
        position++;  // skip closing quote
        return source.substr(start, position - start - 1);
    }

    std::string readIdentifier() {
        size_t start = position;
        while (position < source.length() && (std::isalnum(source[position]) || source[position] == '_')) position++;
        return source.substr(start, position - start);
    }

    Token readOperator() {
        char currentChar = source[position++];
        if ((currentChar == '<' || currentChar == '>' || currentChar == '=' || currentChar == '!') &&
            position < source.length() && source[position] == '=') {
            char nextChar = source[position++];
            return {TokenType::Operator, std::string(1, currentChar) + nextChar};
        } else if (currentChar == '+' && position < source.length() && source[position] == '+') {
            char nextChar = source[position++];
            return {TokenType::Operator, std::string(1, currentChar) + nextChar};
        } else if (currentChar == '-' && position < source.length() && source[position] == '-') {
            char nextChar = source[position++];
            return {TokenType::Operator, std::string(1, currentChar) + nextChar};
        }
        return {TokenType::Operator, std::string(1, currentChar)};
    }

    bool isKeyword(const std::string& word) {
        static const std::unordered_map<std::string, bool> keywords = {
                {"def", true}, {"int", true}, {"string", true}, {"float", true},
                {"char", true}, {"print", true}, {"if", true}, {"else", true},
                {"for", true}
        };
        return keywords.find(word) != keywords.end();
    }
};


// ---------------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------


struct ASTNode {
    virtual ~ASTNode() = default;
};

struct Expression : ASTNode {};

struct IntegerLiteral : Expression {
    std::string value;
    IntegerLiteral(const std::string& value) : value(value) {}
};

struct FloatLiteral : Expression {
    std::string value;
    FloatLiteral(const std::string& value) : value(value) {}
};

struct CharacterLiteral : Expression {
    std::string value;
    CharacterLiteral(const std::string& value) : value(value) {}
};

struct StringLiteral : Expression {
    std::string value;
    StringLiteral(const std::string& value) : value(value) {}
};

struct Variable : Expression {
    std::string identifier;
    Variable(const std::string& identifier) : identifier(identifier) {}
};

struct BinaryExpression : Expression {
    std::unique_ptr<Expression> left;
    std::unique_ptr<Expression> right;
    std::string op;
    BinaryExpression(std::unique_ptr<Expression> left, std::unique_ptr<Expression> right, const std::string& op)
            : left(std::move(left)), right(std::move(right)), op(op) {}
};

struct IncrementExpression : Expression {
    std::string identifier;
    std::string op;
    IncrementExpression(const std::string& identifier, const std::string& op)
            : identifier(identifier), op(op) {}
};

struct Assignment : ASTNode {
    std::string identifier;
    std::unique_ptr<Expression> expression;
    Assignment(const std::string& identifier, std::unique_ptr<Expression> expression)
            : identifier(identifier), expression(std::move(expression)) {}
};

struct VariableDeclaration : ASTNode {
    std::string type;
    std::string identifier;
    std::unique_ptr<Expression> expression;
    VariableDeclaration(const std::string& type, const std::string& identifier, std::unique_ptr<Expression> expression)
            : type(type), identifier(identifier), expression(std::move(expression)) {}
};

struct PrintStatement : ASTNode {
    std::unique_ptr<Expression> expression;
    PrintStatement(std::unique_ptr<Expression> expression) : expression(std::move(expression)) {}
};

struct FunctionCall : Expression {
    std::string identifier;
    std::vector<std::unique_ptr<Expression>> arguments;
    FunctionCall(const std::string& identifier, std::vector<std::unique_ptr<Expression>> arguments)
            : identifier(identifier), arguments(std::move(arguments)) {}
};

struct FunctionDefinition : ASTNode {
    std::string identifier;
    std::vector<std::pair<std::string, std::string>> parameters; // pair of <type, identifier>
    std::vector<std::unique_ptr<ASTNode>> body;
    FunctionDefinition(const std::string& identifier, std::vector<std::pair<std::string, std::string>> parameters, std::vector<std::unique_ptr<ASTNode>> body)
            : identifier(identifier), parameters(std::move(parameters)), body(std::move(body)) {}
};

struct IfStatement : ASTNode {
    std::unique_ptr<Expression> condition;
    std::unique_ptr<ASTNode> thenBranch;
    std::unique_ptr<ASTNode> elseBranch;
    IfStatement(std::unique_ptr<Expression> condition, std::unique_ptr<ASTNode> thenBranch, std::unique_ptr<ASTNode> elseBranch = nullptr)
            : condition(std::move(condition)), thenBranch(std::move(thenBranch)), elseBranch(std::move(elseBranch)) {}
};

struct ForLoop : ASTNode {
    std::unique_ptr<VariableDeclaration> initializer;
    std::unique_ptr<Expression> condition;
    std::unique_ptr<ASTNode> increment;
    std::unique_ptr<ASTNode> body;
    ForLoop(std::unique_ptr<VariableDeclaration> initializer, std::unique_ptr<Expression> condition, std::unique_ptr<ASTNode> increment, std::unique_ptr<ASTNode> body)
            : initializer(std::move(initializer)), condition(std::move(condition)), increment(std::move(increment)), body(std::move(body)) {}
};

struct Program : ASTNode {
    std::vector<std::unique_ptr<ASTNode>> statements;
};


// ---------------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------


class Parser {
    std::vector<Token> tokens;
    size_t current;

public:
    Parser(std::vector<Token> tokens) : tokens(std::move(tokens)), current(0) {}

    std::unique_ptr<Program> parse() {
        auto program = std::make_unique<Program>();
        while (!match(TokenType::EndOfFile)) {
            program->statements.push_back(parseStatement());
        }
        return program;
    }

private:
    std::unique_ptr<ASTNode> parseStatement() {
        if (match(TokenType::Keyword, "int") || match(TokenType::Keyword, "float") || match(TokenType::Keyword, "char") || match(TokenType::Keyword, "string")) {
            auto node = parseVariableDeclaration();
            expect(TokenType::Operator, "Expected ';' after variable declaration", ";");
            return node;
        } else if (match(TokenType::Keyword, "def")) {
            return parseFunctionDefinition();
        } else if (match(TokenType::Keyword, "if")) {
            return parseIfStatement();
        } else if (match(TokenType::Keyword, "for")) {
            return parseForLoop();
        } else if (match(TokenType::Keyword, "print")) {
            auto node = parsePrintStatement();
            expect(TokenType::Operator, "Expected ';' after print statement", ";");
            return node;
        } else if (match(TokenType::Identifier)) {
            if (lookahead().value == "(") {
                auto node =  parseFunctionCall();
                expect(TokenType::Operator, "Expected ';' after function call", ";");
                return node;
            } else if (lookahead().value == "++" || lookahead().value == "--") {
                auto node = parseIncrementExpression();
                return node;
            } else {
                auto node = parseAssignment();
                expect(TokenType::Operator, "Expected ';' after assignment", ";");
                return node;
            }
        } else {
            throw std::runtime_error("Unexpected token");
        }
    }

    std::unique_ptr<IncrementExpression> parseIncrementExpression() {
        std::string identifier = previous().value;
        std::string op = "++";
        expect(TokenType::Operator, "Expected '++'", "++");
        return std::make_unique<IncrementExpression>(identifier, op);
    }

    std::unique_ptr<VariableDeclaration> parseVariableDeclaration() {
        std::string type = previous().value;
        std::string identifier = expect(TokenType::Identifier, "Expected identifier").value;
        expect(TokenType::Operator, "Expected '='", "=");
        auto expression = parseExpression();
        return std::make_unique<VariableDeclaration>(type, identifier, std::move(expression));
    }

    std::unique_ptr<FunctionDefinition> parseFunctionDefinition() {
        std::string identifier = expect(TokenType::Identifier, "Expected function name").value;
        expect(TokenType::Operator, "Expected '('", "(");
        std::vector<std::pair<std::string, std::string>> parameters;
        if (!check(TokenType::Operator, ")")) {
            do {
                std::string paramType = expect(TokenType::Keyword, "Expected parameter type").value;
                std::string paramName = expect(TokenType::Identifier, "Expected parameter name").value;
                parameters.emplace_back(paramType, paramName);
            } while (match(TokenType::Operator, ","));
        }
        expect(TokenType::Operator, "Expected ')'", ")");
        expect(TokenType::Operator, "Expected '{'", "{");
        std::vector<std::unique_ptr<ASTNode>> body;
        while (!match(TokenType::Operator, "}")) {
            body.push_back(parseStatement());
        }
        return std::make_unique<FunctionDefinition>(identifier, std::move(parameters), std::move(body));
    }

    std::unique_ptr<PrintStatement> parsePrintStatement() {
        expect(TokenType::Operator, "Expected '(' after 'print'", "(");
        auto expr = parseExpression();
        expect(TokenType::Operator, "Expected ')' after expression", ")");
        return std::make_unique<PrintStatement>(std::move(expr));
    }

    std::unique_ptr<Assignment> parseAssignment() {
        std::string identifier = previous().value;
        expect(TokenType::Operator, "Expected '='", "=");
        auto expression = parseExpression();
        return std::make_unique<Assignment>(identifier, std::move(expression));
    }

    std::unique_ptr<FunctionCall> parseFunctionCall() {
        std::string identifier = previous().value;
        expect(TokenType::Operator, "Expected '('", "(");
        std::vector<std::unique_ptr<Expression>> arguments;
        if (!check(TokenType::Operator, ")")) {
            do {
                arguments.push_back(parseExpression());
            } while (match(TokenType::Operator, ","));
        }
        expect(TokenType::Operator, "Expected ')'", ")");
        return std::make_unique<FunctionCall>(identifier, std::move(arguments));
    }

    std::unique_ptr<IfStatement> parseIfStatement() {
        expect(TokenType::Operator, "Expected '(' after 'if'", "(");
        auto condition = parseExpression();
        expect(TokenType::Operator, "Expected ')' after condition", ")");
        expect(TokenType::Operator, "Expected '{'", "{");
        auto thenBranch = parseStatement();
        expect(TokenType::Operator, "Expected '}'", "}");
        std::unique_ptr<ASTNode> elseBranch = nullptr;
        if (match(TokenType::Keyword, "else")) {
            expect(TokenType::Operator, "Expected '{'", "{");
            elseBranch = parseStatement();
            expect(TokenType::Operator, "Expected '}'", "}");
        }
        return std::make_unique<IfStatement>(std::move(condition), std::move(thenBranch), std::move(elseBranch));
    }

    std::unique_ptr<ForLoop> parseForLoop() {
        expect(TokenType::Operator, "Expected '(' after 'for'", "(");
        //match(TokenType::Keyword, "int") || match(TokenType::Keyword, "float");
        expect(TokenType::Keyword, "Expected 'int'");
        auto initializer = parseVariableDeclaration();
        expect(TokenType::Operator, "Expected ';'", ";");
        auto condition = parseExpression();
        expect(TokenType::Operator, "Expected ';'", ";");
        auto increment = parseStatement();  // struggles with x++
        expect(TokenType::Operator, "Expected ')' after loop increments", ")");
        expect(TokenType::Operator, "Expected '{'", "{");
        auto body = parseStatement();
        expect(TokenType::Operator, "Expected '}'", "}");
        return std::make_unique<ForLoop>(std::move(initializer), std::move(condition), std::move(increment), std::move(body));
    }

//    std::unique_ptr<Expression> parseExpression() {
//        if (match(TokenType::Integer)) {
//            return std::make_unique<IntegerLiteral>(previous().value);
//        } else if (match(TokenType::Float)) {
//            return std::make_unique<FloatLiteral>(previous().value);
//        } else if (match(TokenType::Character)) {
//            return std::make_unique<CharacterLiteral>(previous().value);
//        } else if (match(TokenType::String)) {
//            return std::make_unique<StringLiteral>(previous().value);
//        } else if (match(TokenType::Identifier)) {
//            if (lookahead().value == "(") {
//                return parseFunctionCall();
//            } else {
//                return std::make_unique<Variable>(previous().value);
//            }
//        } else if (match(TokenType::Operator) && previous().value == "+") {
//            auto left = parseExpression();
//            auto right = parseExpression();
//            return std::make_unique<BinaryExpression>(std::move(left), std::move(right), "+");
//        } else {
//            throw std::runtime_error("Unexpected expression token");
//        }
//    }

    std::unique_ptr<Expression> parseExpression() {
        return parseEquality();
    }

    std::unique_ptr<Expression> parseEquality() {
        auto expr = parseComparison();
        while (match(TokenType::Operator, "==") || match(TokenType::Operator, "!=")) {
            std::string op = previous().value;
            auto right = parseComparison();
            expr = std::make_unique<BinaryExpression>(std::move(expr), std::move(right), op);
        }
        return expr;
    }

    std::unique_ptr<Expression> parseComparison() {
        auto expr = parseAddition();
        while (match(TokenType::Operator, "<") || match(TokenType::Operator, "<=") ||
               match(TokenType::Operator, ">") || match(TokenType::Operator, ">=")) {
            std::string op = previous().value;
            auto right = parseAddition();
            expr = std::make_unique<BinaryExpression>(std::move(expr), std::move(right), op);
        }
        return expr;
    }

    std::unique_ptr<Expression> parseAddition() {
        auto expr = parseMultiplication();
        while (match(TokenType::Operator, "+") && lookahead().value != "+" ||
                match(TokenType::Operator, "-") && lookahead().value != "-") {
            std::string op = previous().value;
            auto right = parseMultiplication();
            expr = std::make_unique<BinaryExpression>(std::move(expr), std::move(right), op);
        }
        return expr;
    }

    std::unique_ptr<Expression> parseMultiplication() {
        auto expr = parsePrimary();
        while (match(TokenType::Operator, "*") || match(TokenType::Operator, "/")) {
            std::string op = previous().value;
            auto right = parsePrimary();
            expr = std::make_unique<BinaryExpression>(std::move(expr), std::move(right), op);
        }
        return expr;
    }

//    std::unique_ptr<Expression> parseIncrementExpression() {
//        auto expr = parsePrimary();
//        if (match(TokenType::Operator, "++")) {
//            expr = std::make_unique<PostIncrementExpression>(std::move(expr));
//        } else if (match(TokenType::Operator, "--")) {
//            expr = std::make_unique<PostDecrementExpression>(std::move(expr));
//        }
//        return expr;
//    }

    std::unique_ptr<Expression> parsePrimary() {
        if (match(TokenType::Integer)) {
            return std::make_unique<IntegerLiteral>(previous().value);
        } else if (match(TokenType::Float)) {
            return std::make_unique<FloatLiteral>(previous().value);
        } else if (match(TokenType::Character)) {
            return std::make_unique<CharacterLiteral>(previous().value);
        } else if (match(TokenType::String)) {
            return std::make_unique<StringLiteral>(previous().value);
        } else if (match(TokenType::Identifier)) {
            if (lookahead().value == "(") {
                return parseFunctionCall();
            } else {
                return std::make_unique<Variable>(previous().value);
            }
        } else {
            throw std::runtime_error("Unexpected expression token");
        }
    }

    bool match(TokenType type, const std::string& value = "") {
        if (current < tokens.size() && tokens[current].type == type &&
            (value.empty() || tokens[current].value == value)) {
            current++;
            return true;
        }
        return false;
    }

    Token& lookahead(int offset = 0) {
        static Token eof{TokenType::EndOfFile, ""};
        if (current + offset < tokens.size()) {
            return tokens[current + offset];
        }
        return eof;
    }

    Token expect(TokenType type, const std::string& errorMessage, const std::string& value = "") {
        if (current < tokens.size() && tokens[current].type == type &&
            (value.empty() || tokens[current].value == value)) {
            return tokens[current++];
        } else {
            std::stringstream ss;
            ss << errorMessage << " Found '" << (current < tokens.size() ? tokens[current].value : "<EOF>") << "' at position " << current;
            throw std::runtime_error(ss.str());
        }
    }


    Token previous() {
        return tokens[current - 1];
    }

    bool check(TokenType type, const std::string& value = "") {
        if (current < tokens.size() && tokens[current].type == type &&
            (value.empty() || tokens[current].value == value)) {
            return true;
        }
        return false;
    }
};


// ---------------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------



class SemanticAnalyzer {
    std::unordered_map<std::string, std::string> variableTypes;
    std::unordered_map<std::string, std::vector<std::pair<std::string, std::string>>> functionSignatures;

public:
    void analyze(std::unique_ptr<Program>& program) {
        for (const auto& stmt : program->statements) {
            analyzeStatement(*stmt);
        }
    }

private:
    void analyzeStatement(const ASTNode& node) {
        if (const auto* varDecl = dynamic_cast<const VariableDeclaration*>(&node)) {
            if (variableTypes.find(varDecl->identifier) != variableTypes.end()) {
                throw std::runtime_error("Variable redeclaration: " + varDecl->identifier);
            }
            variableTypes[varDecl->identifier] = varDecl->type;
            analyzeExpression(*varDecl->expression);
        } else if (const auto* funcDef = dynamic_cast<const FunctionDefinition*>(&node)) {
            if (functionSignatures.find(funcDef->identifier) != functionSignatures.end()) {
                throw std::runtime_error("Function redefinition: " + funcDef->identifier);
            }
            functionSignatures[funcDef->identifier] = funcDef->parameters;
            for (const auto& stmt : funcDef->body) {
                analyzeStatement(*stmt);
            }
        } else if (const auto* printStmt = dynamic_cast<const PrintStatement*>(&node)) {
            analyzeExpression(*printStmt->expression);
        } else if (const auto* assign = dynamic_cast<const Assignment*>(&node)) {
            if (variableTypes.find(assign->identifier) == variableTypes.end()) {
                throw std::runtime_error("Assignment to undeclared variable: " + assign->identifier);
            }
            analyzeExpression(*assign->expression);
        } else if (const auto* printStmt = dynamic_cast<const PrintStatement*>(&node)) {
            analyzeExpression(*printStmt->expression);
        } else if (const auto* ifStmt = dynamic_cast<const IfStatement*>(&node)) {
            analyzeExpression(*ifStmt->condition);
            analyzeStatement(*ifStmt->thenBranch);
            if (ifStmt->elseBranch) {
                analyzeStatement(*ifStmt->elseBranch);
            }
        } else if (const auto* forLoop = dynamic_cast<const ForLoop*>(&node)) {
            analyzeStatement(*forLoop->initializer);
            analyzeExpression(*forLoop->condition);
            analyzeStatement(*forLoop->increment);
            analyzeStatement(*forLoop->body);
        }
    }

    void analyzeExpression(const Expression& expr) {
        if (const auto* intLit = dynamic_cast<const IntegerLiteral*>(&expr)) {
            // No need to check integer literals
        } else if (const auto* floatLit = dynamic_cast<const FloatLiteral*>(&expr)) {
            // No need to check float literals
        } else if (const auto* charLit = dynamic_cast<const CharacterLiteral*>(&expr)) {
            // No need to check character literals
        } else if (const auto* strLit = dynamic_cast<const StringLiteral*>(&expr)) {
            // No need to check string literals
        } else if (const auto* var = dynamic_cast<const Variable*>(&expr)) {
            if (variableTypes.find(var->identifier) == variableTypes.end()) {
                throw std::runtime_error("Use of undeclared variable: " + var->identifier);
            }
        } else if (const auto* binExpr = dynamic_cast<const BinaryExpression*>(&expr)) {
            analyzeExpression(*binExpr->left);
            analyzeExpression(*binExpr->right);
        } else if (const auto* funcCall = dynamic_cast<const FunctionCall*>(&expr)) {
            if (functionSignatures.find(funcCall->identifier) == functionSignatures.end()) {
                throw std::runtime_error("Call to undefined function: " + funcCall->identifier);
            }
            const auto& params = functionSignatures[funcCall->identifier];
            if (params.size() != funcCall->arguments.size()) {
                throw std::runtime_error("Incorrect number of arguments for function: " + funcCall->identifier);
            }
            for (size_t i = 0; i < params.size(); i++) {
                analyzeExpression(*funcCall->arguments[i]);
            }
        }
    }
};


// ---------------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------


class CodeGenerator {
public:
    std::string generateCode(const Program& program) {
        std::stringstream ss;
        // Include necessary C++ headers and use the standard namespace
        ss << "#include <iostream>\n#include <string>\nusing namespace std;\n\n";
        for (const auto& stmt : program.statements) {
            generateStatement(ss, *stmt);
        }
        return ss.str();
    }

private:
    void generateStatement(std::stringstream& ss, const ASTNode& node) {
        if (const auto* varDecl = dynamic_cast<const VariableDeclaration*>(&node)) {
            ss << varDecl->type << " " << varDecl->identifier << " = ";
            generateExpression(ss, *varDecl->expression);
            ss << ";\n";
        } else if (const auto* funcDef = dynamic_cast<const FunctionDefinition*>(&node)) {
            if (funcDef->identifier == "main") {
                ss << "int " << funcDef->identifier << "() {\n";
                for (const auto &stmt: funcDef->body) {
                    generateStatement(ss, *stmt);
                }
                ss << "}\n";
            } else {
                ss << "void " << funcDef->identifier << "() {\n";
                for (const auto &stmt: funcDef->body) {
                    generateStatement(ss, *stmt);
                }
                ss << "}\n";
            }
        } else if (const auto* assign = dynamic_cast<const Assignment*>(&node)) {
            ss << assign->identifier << " = ";
            generateExpression(ss, *assign->expression);
            ss << ";\n";
        } else if (const auto* printStmt = dynamic_cast<const PrintStatement*>(&node)) {
            ss << "std::cout << ";
            generateExpression(ss, *printStmt->expression);
            ss << " << std::endl;\n";
        } else if (const auto* funcCall = dynamic_cast<const FunctionCall*>(&node)) {
            ss << funcCall->identifier << "();\n";
        } else if (const auto* ifStmt = dynamic_cast<const IfStatement*>(&node)) {
            ss << "if (";
            generateExpression(ss, *ifStmt->condition);
            ss << ") {\n";
            generateStatement(ss, *ifStmt->thenBranch);
            ss << "}\n";
            if (ifStmt->elseBranch) {
                ss << "else {\n";
                generateStatement(ss, *ifStmt->elseBranch);
                ss << "}\n";
            }
        } else if (const auto* forLoop = dynamic_cast<const ForLoop*>(&node)) {
            ss << "for (";
            generateStatement(ss, *forLoop->initializer);
            ss.seekp(-2, std::ios_base::end);  // Remove the last semicolon for correct syntax
            ss << "; ";
            generateExpression(ss, *forLoop->condition);
            ss << "; ";
            const auto* increment = dynamic_cast<const IncrementExpression*>(&*forLoop->increment);
            ss << increment->identifier << translateOperator(increment->op);
            ss << ") {\n";
            generateStatement(ss, *forLoop->body);
            ss << "}\n";
        }
    }

    void generateExpression(std::stringstream& ss, const Expression& expr) {
        if (const auto* intLit = dynamic_cast<const IntegerLiteral*>(&expr)) {
            ss << intLit->value;
        } else if (const auto* floatLit = dynamic_cast<const FloatLiteral*>(&expr)) {
            ss << floatLit->value;
        } else if (const auto* charLit = dynamic_cast<const CharacterLiteral*>(&expr)) {
            ss << "'" << charLit->value << "'";
        } else if (const auto* strLit = dynamic_cast<const StringLiteral*>(&expr)) {
            ss << '"' << strLit->value << '"';
        } else if (const auto* var = dynamic_cast<const Variable*>(&expr)) {
            ss << var->identifier;
        } else if (const auto* binExpr = dynamic_cast<const BinaryExpression*>(&expr)) {
            generateExpression(ss, *binExpr->left);
            ss << " " << translateOperator(binExpr->op) << " ";
            generateExpression(ss, *binExpr->right);
        } else if (const auto* funcCall = dynamic_cast<const FunctionCall*>(&expr)) {
            ss << funcCall->identifier << "(";
            for (size_t i = 0; i < funcCall->arguments.size(); ++i) {
                if (i > 0) ss << ", ";
                generateExpression(ss, *funcCall->arguments[i]);
            }
            ss << ")";
        }
    }

    std::string translateOperator(const std::string& op) {
        if (op == "+") return "+";
        else if (op == "-") return "-";
        else if (op == "*") return "*";
        else if (op == "/") return "/";
        else if (op == "<") return "<";
        else if (op == "<=") return "<=";
        else if (op == ">") return ">";
        else if (op == ">=") return ">=";
        else if (op == "==") return "==";
        else if (op == "!=") return "!=";
        else if (op == "++") return "++";
        return op; // Default case to handle unexpected operators
    }
};




// ---------------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------



int main() {
//    try {
//        std::string source = R"(
//def greet() {
//    print("Hello");
//}
//
//def main() {
//    for (int x = 5; x <= 10; x++) {
//        print("Hello");
//    }
//}
//)";



//        Lexer lexer(source);
//        auto tokens = lexer.tokenize();
//
//        Parser parser(std::move(tokens));
//        auto program = parser.parse();
//
//        SemanticAnalyzer analyzer;
//        analyzer.analyze(program);
//
//        CodeGenerator generator;
//        std::string cppCode = generator.generateCode(*program);
//        // std::cout << cppCode;
//    } catch (const std::exception& e) {
//        std::cerr << "Exception caught: " << e.what() << std::endl;
//    }

    std::ifstream inputFile("/Users/lasha/CLionProjects/CMPLY/input.cmply");
    if (!inputFile.is_open()) {
        std::cerr << "Failed to open input file." << std::endl;
        return 1;
    }

    std::string line, sourceCode;
    while (getline(inputFile, line)) {
        sourceCode += line + "\n";
    }
    inputFile.close();

    Lexer lexer(sourceCode);
    auto tokens = lexer.tokenize();

    Parser parser(std::move(tokens));
    auto program = parser.parse();

    SemanticAnalyzer analyzer;
    analyzer.analyze(program);

    CodeGenerator generator;
    std::string cppCode = generator.generateCode(*program);

    std::ofstream outputFile("../output.cpp");
    if (!outputFile.is_open()) {
        std::cerr << "Failed to open output file." << std::endl;
        return 1;
    }
    outputFile << cppCode;
    outputFile.close();

    // Compile the C++ source code
#if defined(_WIN32) || defined(_WIN64)
    system("g++ ../output.cpp -o ../output.exe"); // Windows compilation
    system("..\\output.exe");                // Windows execution
#else
    system("g++ ../output.cpp -o ../output");     // Unix compilation
    system("../output");                     // Unix execution
#endif

    return 0;
}