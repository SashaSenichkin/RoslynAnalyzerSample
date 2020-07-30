using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Analyzer1
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class Analyzer1Analyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "Analyzer1";

        // You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
        // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Localizing%20Analyzers.md for more on localization
        private static readonly LocalizableString Title = "Title example";
        private static readonly LocalizableString MessageFormat = "Message example. you can put line number here {0}";
        private static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        private const string Category = "Naming";

        private static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault: true, description: Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            // TODO: Consider registering other actions that act on syntax instead of or in addition to symbols
            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information

            context.RegisterSyntaxNodeAction(ExampleReport, SyntaxKind.MethodDeclaration);
            context.RegisterSyntaxNodeAction(CodeSmellsMethod, SyntaxKind.MethodDeclaration);
            context.RegisterSyntaxNodeAction(CodeSmellsClass, SyntaxKind.ClassDeclaration);
            context.RegisterSyntaxNodeAction(NumericLiteralCheck, SyntaxKind.NumericLiteralExpression);
            context.RegisterSyntaxNodeAction(StringReplaceSeeker, SyntaxKind.IdentifierName);
            context.RegisterSyntaxNodeAction(ThrowSeek, SyntaxKind.ThrowExpression, SyntaxKind.ThrowStatement);
            context.RegisterSyntaxNodeAction(ThrowSeekDescent, SyntaxKind.ClassDeclaration);
            context.RegisterSyntaxNodeAction(CheckComments, SyntaxKind.CompilationUnit);
            context.RegisterSyntaxNodeAction(NamesCheck, SyntaxKind.MethodDeclaration);
            context.RegisterSyntaxNodeAction(NamesCheckThrow, SyntaxKind.MethodDeclaration);
            context.RegisterSyntaxNodeAction(DisposableCHeck, SyntaxKind.ObjectCreationExpression);
        }

        private void ExampleReport(SyntaxNodeAnalysisContext obj)
        {
            var diag = Diagnostic.Create(Rule, obj.Node.GetLocation(), GetLineNum(obj.Node));
            obj.ReportDiagnostic(diag);
        }
        static int GetLineNum(SyntaxNode node)
        {
            var sourceText = node.SyntaxTree.GetText();
            return sourceText.Lines.IndexOf(node.SpanStart) + 1;
        }
        private void CodeSmellsMethod(SyntaxNodeAnalysisContext obj)
        {
            var node = obj.Node as MethodDeclarationSyntax;
            if (node.Body?.Statements.Count > 500)//too long method
            {/*Error*/ }
            if (node.ParameterList?.Parameters.Count > 10) //too many params in method
            {/*Error*/}
        }

        private void CodeSmellsClass(SyntaxNodeAnalysisContext obj)
        {
            var node = obj.Node as ClassDeclarationSyntax;
            if (node.Members.Count > 50)//too large class
            {/*Error*/ }
        }

        private void NumericLiteralCheck(SyntaxNodeAnalysisContext obj)
        {
            if (obj.Node.Ancestors().OfType<VariableDeclaratorSyntax>().Any())
            { return; }

            var node = obj.Node as LiteralExpressionSyntax;
            if (int.TryParse(node.Token.ValueText, out int num))
            {/*Error*/}

            if (num > 10 && num % 10 == 0) // exclude pattern
            {/*Error*/}
        }

        private void StringReplaceSeeker(SyntaxNodeAnalysisContext obj)
        {
            var node = obj.Node as IdentifierNameSyntax;
            var model = obj.SemanticModel;
            if (node.Identifier.Text == "Replace")
            {/*Error*/}

            var member = node.Parent as MemberAccessExpressionSyntax;
            if (member == null)
                return;

            var baseType = (model.GetTypeInfo(member.Expression).Type)?.BaseType;
            if (member != null
                && baseType.Name == nameof(String)
                && baseType.MetadataName == typeof(String).FullName)
            {/*Error*/}
        }

        private void ThrowSeek(SyntaxNodeAnalysisContext obj)
        {
            if (obj.Node.Ancestors().OfType<ClassDeclarationSyntax>()
                   .First().Identifier.Text != "MyThrowerClass")
            {/*Error*/}
        }

        private void ThrowSeekDescent(SyntaxNodeAnalysisContext obj)
        {
            var node = obj.Node as ClassDeclarationSyntax;
            if (node.Identifier.Text == "MyThrowerClass")
            { return; }

            if (node.DescendantTrivia().Any(x => x.Kind() == SyntaxKind.ThrowKeyword))
            {/*Error*/}

            if (node.DescendantNodes().Any(x => x.Kind() == SyntaxKind.ThrowExpression || x.Kind() == SyntaxKind.ThrowStatement))
            {/*Error*/}
        }

        private void CheckComments(SyntaxNodeAnalysisContext obj)
        {
            var comments = obj.Node.DescendantTrivia().Where(x =>
                            x.Kind() == SyntaxKind.SingleLineCommentTrivia
                         || x.Kind() == SyntaxKind.MultiLineCommentTrivia);

            var words = new string[] { "#$@*$", "@#!*#$" };
            foreach (var comment in comments)
            {
                var commWords = comment.Token.Text.Split(' ');
                if (commWords.Any(x => words.Contains(x)))
                {/*Error*/}
            }
        }

        private void NamesCheck(SyntaxNodeAnalysisContext obj)
        {
            var node = obj.Node as MethodDeclarationSyntax;
            if (node.Identifier.Text.StartsWith("Is")
                && node.ReturnType is PredefinedTypeSyntax predef
                && predef.Keyword.Text != "bool")
            {/*Error*/}

            if (node.Identifier.Text.StartsWith("Get")
                && node.ReturnType is PredefinedTypeSyntax predef1
                && predef1.Keyword.Text == "void")
            {/*Error*/}

            if (node.Identifier.Text.StartsWith("Set")
                && node.ReturnType is PredefinedTypeSyntax predef2
                && (predef2.Keyword.Text != "void"
                || !node.ParameterList.Parameters.Any()))
            {/*Error*/}
        }

        private void NamesCheckThrow(SyntaxNodeAnalysisContext obj)
        {
            var node = obj.Node as MethodDeclarationSyntax;

            if (!node.Identifier.Text.Contains("Throw"))
            { return; }

            if (!node.DescendantNodes().Any(x => x is ThrowExpressionSyntax
                                              || x is ThrowStatementSyntax))
            {/*Error*/}


            if (!node.Body.Statements.Any(x => x is ThrowStatementSyntax))
            {/*Error*/}

        }

        private void DisposableCHeck(SyntaxNodeAnalysisContext obj)
        {
            var node = obj.Node;
            var model = obj.SemanticModel;
            if (!model.GetTypeInfo(node).Type.Interfaces.Any(x => x.Name == nameof(IDisposable)))
                return;

            if (!node.Ancestors().Any(x => x is UsingStatementSyntax))
            {/*Error*/}
        }
    }
}
