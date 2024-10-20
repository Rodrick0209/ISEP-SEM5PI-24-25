using System;
using Xunit;
using DDDSample1.Domain.User;

namespace MastersData.test.UnitTest.Domain.User
{
    public class UserTest
    {
        [Fact]
        public void TestUserCreationWithEmailAndRole()
        {
            // Arrange
            string email = "test@example.com";
            string role = "doctor";

            // Act
            DDDSample1.Domain.User.User user = new DDDSample1.Domain.User.User(email, role);

            // Assert
            Assert.NotNull(user.Id);
            Assert.Equal(email, user.email.email);
            Assert.Equal(role, user.role.role);
            Assert.NotNull(user.loginFailCounter);
            Assert.Equal(0, user.loginFailCounter.loginFailCounter);
        }

        [Fact]
        public void TestUserCreationWithEmailRoleAndPassword()
        {
            // Arrange
            string email = "test@example.com";
            string role = "doctor";
            string password = "Password123";

            // Act
            DDDSample1.Domain.User.User user = new DDDSample1.Domain.User.User(email, role, password);

            // Assert
            Assert.NotNull(user.Id);
            Assert.Equal(email, user.email.email);
            Assert.Equal(role, user.role.role);
            Assert.Equal(password, user.password.password);
            Assert.NotNull(user.loginFailCounter);
            Assert.Equal(0, user.loginFailCounter.loginFailCounter);
            Assert.NotNull(user.accountConfirmed);
            Assert.False(user.accountConfirmed);
        }

        [Fact]
        public void TestSetPassword()
        {
            // Arrange
            string email = "test@example.com";
            string role = "doctor";
            DDDSample1.Domain.User.User user = new DDDSample1.Domain.User.User(email, role);
            string newPassword = "NewPassword123";

            // Act
            user.SetPassword(newPassword);

            // Assert
            Assert.Equal(newPassword, user.password.password);
        }

        [Fact]
        public void TestSetResetPasswordToken()
        {
            // Arrange
            string email = "test@example.com";
            string role = "doctor";
            DDDSample1.Domain.User.User user = new DDDSample1.Domain.User.User(email, role);
            string token = "resetToken";
            DateTime expirationDate = DateTime.UtcNow.AddHours(1);

            // Act
            user.SetResetPasswordToken(token, expirationDate);

            // Assert
            Assert.NotNull(user.resetPasswordToken);
            Assert.Equal(token, user.resetPasswordToken.resetPasswordToken);
            Assert.NotNull(user.resetPasswordTokenExpiration);
            Assert.Equal(expirationDate, user.resetPasswordTokenExpiration.resetPasswordTokenExpiration);
        }

        [Fact]
        public void TestClearResetPasswordToken()
        {
            // Arrange
            string email = "test@example.com";
            string role = "doctor";
            DDDSample1.Domain.User.User user = new DDDSample1.Domain.User.User(email, role);
            user.SetResetPasswordToken("resetToken", DateTime.UtcNow.AddHours(1));

            // Act
            user.ClearResetPasswordToken();

            // Assert
            Assert.Null(user.resetPasswordToken);
            Assert.Null(user.resetPasswordTokenExpiration);
        }

        [Fact]
        public void TestIncreaseFailCounter()
        {
            // Arrange
            string email = "test@example.com";
            string role = "doctor";
            DDDSample1.Domain.User.User user = new DDDSample1.Domain.User.User(email, role);
            int maxAllowedFailCounter = 3;
            int minDefinedAsLimit = 15;

            // Act
            user.IncreaseFailCounter(maxAllowedFailCounter, minDefinedAsLimit);
            user.IncreaseFailCounter(maxAllowedFailCounter, minDefinedAsLimit);
            user.IncreaseFailCounter(maxAllowedFailCounter, minDefinedAsLimit);

            // Assert
            Assert.NotNull(user.loginFailCounter);
            Assert.Equal(0, user.loginFailCounter.loginFailCounter);
            Assert.NotNull(user.accountBlockedTime);
        }

        [Fact]
        public void TestResetFailCounter()
        {
            // Arrange
            string email = "test@example.com";
            string role = "doctor";
            DDDSample1.Domain.User.User user = new DDDSample1.Domain.User.User(email, role);
            user.IncreaseFailCounter(3, 15);

            // Act
            user.ResetFailCounter();

            // Assert
            Assert.Null(user.loginFailCounter);
        }

        [Fact]
        public void TestSetAccountBlockedTime()
        {
            // Arrange
            string email = "test@example.com";
            string role = "doctor";
            DDDSample1.Domain.User.User user = new DDDSample1.Domain.User.User(email, role);
            int minDefinedAsLimit = 15;

            // Act
            user.SetAccountBlockedTime(minDefinedAsLimit);

            // Assert
            Assert.NotNull(user.accountBlockedTime);
        }

        [Fact]
        public void TestClearAccountBlockedTime()
        {
            // Arrange
            string email = "test@example.com";
            string role = "doctor";
            DDDSample1.Domain.User.User user = new DDDSample1.Domain.User.User(email, role);
            user.SetAccountBlockedTime(15);

            // Act
            user.ClearAccountBlockedTime();

            // Assert
            Assert.Null(user.accountBlockedTime);
        }

        [Fact]
        public void TestCheckIfAccountIsBlocked()
        {
            // Arrange
            string email = "test@example.com";
            string role = "doctor";
            DDDSample1.Domain.User.User user = new DDDSample1.Domain.User.User(email, role);
            user.SetAccountBlockedTime(15);

            // Act
            bool isBlocked = user.checkIfAccountIsBlocked();

            // Assert
            Assert.True(isBlocked);
        }

        [Fact]
        public void TestConfirmAccount()
        {
            // Arrange
            string email = "test@example.com";
            string role = "doctor";
            DDDSample1.Domain.User.User user = new DDDSample1.Domain.User.User(email, role);

            // Act
            user.ConfirmAccount();

            // Assert
            Assert.NotNull(user.accountConfirmed);
            Assert.True(user.accountConfirmed);
            Assert.Null(user.confirmationRegisterPatientToken);
            Assert.Null(user.confirmationRegisterPatientTokenExpiration);
        }
    }
}