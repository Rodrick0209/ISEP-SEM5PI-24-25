using System;
using Xunit;
using DDDSample1.Domain.Patients;

namespace UnitTest.Domain.Patient
{
    public class EmergencyContactTest
    {
        [Fact]
        public void CreateEmergencyContact_ValidContact_ShouldCreateSuccessfully()
        {
            // Arrange
            string validContact = "934212356";

            // Act
            var emergencyContact = new EmergencyContact(validContact);

            // Assert
            Assert.Equal(validContact, emergencyContact.emergencyContact);
        }

        [Fact]
        public void CreateEmergencyContact_NullOrEmptyContact_ShouldThrowArgumentNullException()
        {
            // Arrange
            string? invalidContact = null;

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new EmergencyContact(invalidContact));
        }

        [Fact]
        public void CreateEmergencyContact_EmptyContact_ShouldThrowArgumentNullException()
        {
            // Arrange
            string invalidContact = "";

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new EmergencyContact(invalidContact));
        }
    }
}