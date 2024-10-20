using System;
using DDDSample1.Domain.User;
using DDDSample1.Domain.Shared;
using Xunit;

namespace MastersData.test.UnitTest.Domain.User
{
    public class PasswordTest
    {
        [Fact]
        public void Should_Create_Password_When_Valid()
        {
            // Arrange
            var validPassword = "ValidPassword123";

            // Act
            var password = new Password(validPassword);

            // Assert
            Assert.Equal(validPassword, password.password);
        }

        [Fact]
        public void Should_Throw_Exception_When_Password_Is_Null()
        {
            // Arrange
            string? invalidPassword = null;

            // Act & Assert
            Assert.Throws<BusinessRuleValidationException>(() => new Password(invalidPassword));
        }

        [Fact]
        public void Should_Throw_Exception_When_Password_Is_Empty()
        {
            // Arrange
            var invalidPassword = "";

            // Act & Assert
            Assert.Throws<BusinessRuleValidationException>(() => new Password(invalidPassword));
        }

        [Fact]
        public void Should_Throw_Exception_When_Password_Is_Whitespace()
        {
            // Arrange
            var invalidPassword = "   ";

            // Act & Assert
            Assert.Throws<BusinessRuleValidationException>(() => new Password(invalidPassword));
        }
    }
}