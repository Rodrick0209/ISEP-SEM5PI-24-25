using System;
using DDDSample1.Domain.Utils;
using Xunit;

namespace MastersData.test.UnitTest.Domain.Utils
{
    public class EmailTest
    {
        [Fact]
        public void CreateEmail_WithValidEmail_ShouldSetEmail()
        {
            // Arrange
            var validEmail = "test@example.com";

            // Act
            var email = new Email(validEmail);

            // Assert
            Assert.Equal(validEmail, email.email);
        }

        [Fact]
        public void CreateEmail_WithNullEmail_ShouldThrowArgumentNullException()
        {
            // Arrange
            string? nullEmail = null;

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new Email(nullEmail));
        }

        [Fact]
        public void CreateEmail_WithEmptyEmail_ShouldThrowArgumentNullException()
        {
            // Arrange
            var emptyEmail = "";

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new Email(emptyEmail));
        }

        [Fact]
        public void CreateEmail_WithWhitespaceEmail_ShouldThrowArgumentNullException()
        {
            // Arrange
            var whitespaceEmail = "   ";

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new Email(whitespaceEmail));
        }
    }
}