using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.User;
using DDDSample1.Infrastructure;
using DDDSample1.Infrastructure.Users;
using Microsoft.Extensions.Configuration;
using Moq;
using Xunit;

namespace MastersData.test.UnitTest.Domain.User
{
    public class UserServiceTest
    {
        private readonly Mock<IUnitOfWork> _unitOfWorkMock;
        private readonly Mock<IUserRepository> _userRepoMock;
        private readonly Mock<IEmailSender> _emailSenderMock;
        private readonly Mock<IConfiguration> _configurationMock;
        private readonly Mock<IPatientRepository> _patientRepoMock;
        private readonly UserService _userService;

        public UserServiceTest()
        {
            _unitOfWorkMock = new Mock<IUnitOfWork>();
            _userRepoMock = new Mock<IUserRepository>();
            _emailSenderMock = new Mock<IEmailSender>();
            _configurationMock = new Mock<IConfiguration>();
            _patientRepoMock = new Mock<IPatientRepository>();

            _userService = new UserService(
                _unitOfWorkMock.Object,
                _userRepoMock.Object,
                _configurationMock.Object,
                _emailSenderMock.Object,
                _patientRepoMock.Object
            );
        }

        [Fact]
        public async Task GetLogToken_ShouldThrowException_WhenRequestIsNull()
        {
            await Assert.ThrowsAsync<Exception>(() => _userService.GetLogToken(null));
        }

        [Fact]
        public async Task GetLogToken_ShouldThrowException_WhenUserNotFound()
        {
            var request = new LoginRequest { Email = "test@example.com", Password = "password" };
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(request.Email)).ReturnsAsync(default(DDDSample1.Domain.User.User));

            await Assert.ThrowsAsync<Exception>(() => _userService.GetLogToken(request));
        }

        [Fact]
        public async Task GetLogToken_ShouldThrowException_WhenAccountIsBlocked()
        {
            var request = new LoginRequest { Email = "test@example.com", Password = "password" };
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(request.Email)).ReturnsAsync(user);
            user.SetAccountBlockedTime(10);

            await Assert.ThrowsAsync<Exception>(() => _userService.GetLogToken(request));
        }

        [Fact]
        public async Task AddAsync_ShouldThrowException_WhenEmailExists()
        {
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            _userRepoMock.Setup(repo => repo.CheckEmail(user.email.email)).ReturnsAsync(true);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.AddAsync(user));
        }

        [Fact]
        public async Task AddAsync_ShouldAddUser_WhenEmailDoesNotExist()
        {
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            _userRepoMock.Setup(repo => repo.CheckEmail(user.email.email)).ReturnsAsync(false);

            var result = await _userService.AddAsync(user);

            _userRepoMock.Verify(repo => repo.AddAsync(user), Times.Once);
            _unitOfWorkMock.Verify(uow => uow.CommitAsync(), Times.Once);
            Assert.Equal(user, result);
        }

        [Fact]
        public async Task GetAllAsync_ShouldReturnAllUsers()
        {
            var users = new List<DDDSample1.Domain.User.User> {
                new DDDSample1.Domain.User.User("test@example.com", "patient"),
                new DDDSample1.Domain.User.User("test1@example.com", "nurse"),
            };

            _userRepoMock.Setup(repo => repo.GetAllAsync()).ReturnsAsync(users);

            var result = await _userService.GetAllAsync();

            Assert.Equal(users, result);
        }

        [Fact]
        public async Task GetByEmailAsync_ShouldReturnUser_WhenUserExists()
        {
            PasswordHasher passwordHasher = new PasswordHasher();
            var email = "test@example.com";
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(email)).ReturnsAsync(user);

            var result = await _userService.GetByEmailAsync(email);

            Assert.Equal(user, result);
        }
        
        [Fact]
        public async Task SendEmailForAbusiveAccountAccess_ShouldSendEmail()
        {
            var email = "test@example.com";
            _configurationMock.Setup(config => config["Admin:Email"]).Returns("admin@example.com");

            var result = await _userService.SendEmailForAbusiveAccountAccess(email);

            _emailSenderMock.Verify(sender => sender.SendEmailAsync(
                It.IsAny<string>(),
                "admin@example.com",
                "Account Blocked"
            ), Times.Once);

            Assert.Equal("Email sent", result);
        }

        [Fact]
        public async Task ResetPassword_ShouldThrowException_WhenTokenIsInvalid()
        {
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var token = "invalid_token";
            user.SetResetPasswordToken("valid_token", DateTime.UtcNow.AddHours(24));

            await Assert.ThrowsAsync<Exception>(() => _userService.ResetPassword(user, "new_password", token));
        }

        [Fact]
        public async Task ResetPassword_ShouldResetPassword_WhenTokenIsValid()
        {
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var token = "valid_token";
            user.SetResetPasswordToken(token, DateTime.UtcNow.AddHours(24));

            var result = await _userService.ResetPassword(user, "new_password", token);

            _unitOfWorkMock.Verify(uow => uow.CommitAsync(), Times.Once);
            Assert.Equal("Success", result);
        }

        [Fact]
        public async Task sendEmailWithUrlResetPassword_ShouldSendEmail()
        {
            var email = "test@example.com";
            var url = "http://example.com/resetpassword";

            var result = await _userService.sendEmailWithUrlResetPassword(email, url);

            _emailSenderMock.Verify(sender => sender.SendEmailAsync(
                It.IsAny<string>(),
                email,
                "ResetPassword"
            ), Times.Once);

            Assert.Equal("Email sent", result);
        }

        [Fact]
        public async Task RegisterPatientAsync_ShouldThrowException_WhenEmailIsNotUnique()
        {
            var dto = new RegisteringPatientDto { Name = "John Doe", PhoneNumber = "123456789", Email = "test@example.com", Password = "Password123!" };
            _userRepoMock.Setup(repo => repo.CheckEmail(dto.Email)).ReturnsAsync(true);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.RegisterPatientAsync(dto));
        }

        [Fact]
        public async Task RegisterPatientAsync_ShouldThrowException_WhenPatientNotFound()
        {
            var dto = new RegisteringPatientDto { Name = "John Doe", PhoneNumber = "123456789", Email = "test@example.com", Password = "Password123!" };

            _userRepoMock.Setup(repo => repo.CheckEmail(dto.Email)).ReturnsAsync(false);
            _patientRepoMock.Setup(repo => repo.GetByNameEmailPhoneAsync(dto.Name, dto.Email, dto.PhoneNumber)).ReturnsAsync((Patient?)null);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.RegisterPatientAsync(dto));
        }

        [Fact]
        public async Task ConfirmRegisterPatientAsync_ShouldThrowException_WhenUserNotFound()
        {
            var dto = new ConfirmationRegisterPatientDto("token", "test@example.com");

            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(DDDSample1.Domain.User.User));

            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.ConfirmRegisterPatientAsync(dto));
        }

        [Fact]
        public async Task ConfirmRegisterPatientAsync_ShouldThrowException_WhenPatientNotFound()
        {
            var dto = new ConfirmationRegisterPatientDto("token", "test@example.com");
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var token = "valid_token";
            user.SetConfirmationRegisterPatientToken(token, DateTime.UtcNow.AddHours(24));
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(user);
            _patientRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(Patient));

            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.ConfirmRegisterPatientAsync(dto));
        }
    }
}