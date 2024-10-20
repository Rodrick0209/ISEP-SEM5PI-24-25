using System;
using DDDSample1.Domain.Utils;
using Xunit;

namespace MastersData.test.UnitTest.Domain.Utils
{
    public class PhoneNumberTest
    {
        [Fact]
        public void Should_Create_PhoneNumber_When_Valid()
        {
            // Arrange
            var validPhoneNumber = "1234567890";

            // Act
            var phoneNumber = new PhoneNumber(validPhoneNumber);

            // Assert
            Assert.Equal(validPhoneNumber, phoneNumber.phoneNumber);
        }

        [Fact]
        public void Should_Throw_ArgumentNullException_When_PhoneNumber_Is_NullOrWhiteSpace()
        {
            // Arrange
            string? invalidPhoneNumber = null;

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new PhoneNumber(invalidPhoneNumber));
        }

        [Fact]
        public void Should_Throw_ArgumentNullException_When_PhoneNumber_Is_Empty()
        {
            // Arrange
            var invalidPhoneNumber = "";

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new PhoneNumber(invalidPhoneNumber));
        }

        [Fact]
        public void Should_Throw_ArgumentNullException_When_PhoneNumber_Is_Whitespace()
        {
            // Arrange
            var invalidPhoneNumber = "   ";

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new PhoneNumber(invalidPhoneNumber));
        }
    }
}