using System;
using DDDSample1.Domain.User;
using Xunit;

namespace MastersData.test.UnitTest.Domain.User
{
    public class UserDTOTest
    {
        [Fact]
        public void TestUserDTOCreation()
        {
            // Arrange
            var id = Guid.NewGuid();
            var role = "Admin";
            var email = "test@example.com";
            var password = "password123";

            // Act
            var userDTO = new UserDTO(id, role, email, password);

            // Assert
            Assert.Equal(id, userDTO.Id);
            Assert.Equal(role, userDTO.role);
            Assert.Equal(email, userDTO.email);
            Assert.Equal(password, userDTO.password);
        }

        [Fact]
        public void TestUserDTOProperties()
        {
            // Arrange
            var id = Guid.NewGuid();
            var role = "User";
            var email = "user@example.com";
            var password = "userpassword";

            // Act
            var userDTO = new UserDTO(id, role, email, password);

            // Assert
            Assert.Equal(id, userDTO.Id);
            Assert.Equal(role, userDTO.role);
            Assert.Equal(email, userDTO.email);
            Assert.Equal(password, userDTO.password);

            // Change properties
            var newRole = "SuperUser";
            var newEmail = "superuser@example.com";
            var newPassword = "superpassword";

            userDTO.role = newRole;
            userDTO.email = newEmail;
            userDTO.password = newPassword;

            // Assert changes
            Assert.Equal(newRole, userDTO.role);
            Assert.Equal(newEmail, userDTO.email);
            Assert.Equal(newPassword, userDTO.password);
        }
    }
}