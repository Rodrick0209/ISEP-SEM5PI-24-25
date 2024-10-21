using System;
using Xunit;
using DDDSample1.Domain.User;
using DDDSample1.Domain.Utils;

namespace MastersData.test.UnitTest.Domain.User
{
    public class UserMapperTest
    {
        [Fact]
        public void ToDto_ShouldMapUserToUserDTO()
        {
            // Arrange
            var role = new Role("doctor");
            var email = new Email("test@example.com");
            var password = new Password("password123");
            var user = new DDDSample1.Domain.User.User(email.email, role.role, password.password);

            // Act
            var userDto = UserMapper.ToDto(user);

            // Assert
            Assert.Equal(user.Id.AsGuid(), userDto.Id);
            Assert.Equal(role.role, userDto.role);
            Assert.Equal(email.email, userDto.email);
            Assert.Equal(password.password, userDto.password);
        }

        [Fact]
        public void ToDomain_ShouldMapUserDTOToUser()
        {
            // Arrange
            var userId = Guid.NewGuid();
            var role = "doctor";
            var email = "test@example.com";
            var password = "password123";
            var userDto = new UserDTO(userId, role, email, password);

            // Act
            var user = UserMapper.ToDomain(userDto);

            // Assert
            Assert.Equal(email, user.email.email);
            Assert.Equal(role, user.role.role);
            Assert.NotNull(user.password.password);
        }
    }
}