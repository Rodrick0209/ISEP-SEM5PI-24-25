using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.PatientLoggers;
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
        private Mock<IUnitOfWork>? _unitOfWorkMock;
        private Mock<IUserRepository>? _userRepoMock;
        private Mock<IEmailSender>? _emailSenderMock;
        private Mock<IConfiguration>? _configurationMock;
        private Mock<IPatientRepository>? _patientRepoMock;
        private UserService? _userService;

        [Fact]
        public async Task GetLogToken_ShouldThrowException_WhenRequestIsNull()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);
            await Assert.ThrowsAsync<Exception>(() => _userService.GetLogToken(null));
        }

        [Fact]
        public async Task GetLogToken_ShouldThrowException_WhenUserNotFound()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);
            var request = new LoginRequest { Email = "test@example.com", Password = "password" };
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(request.Email)).ReturnsAsync(default(DDDSample1.Domain.User.User));

            await Assert.ThrowsAsync<Exception>(() => _userService.GetLogToken(request));
        }

        [Fact]
        public async Task GetLogToken_ShouldThrowException_WhenAccountIsBlocked()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);
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
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            _userRepoMock.Setup(repo => repo.CheckEmail(user.email.email)).ReturnsAsync(true);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.AddAsync(user));
        }

        [Fact]
        public async Task AddAsync_ShouldAddUser_WhenEmailDoesNotExist()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _unitOfWorkMock = new Mock<IUnitOfWork>();
            _userService = new UserService(_unitOfWorkMock.Object, _userRepoMock.Object, null, null, null, null);
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
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);
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
            _userRepoMock = new Mock<IUserRepository>();
            _unitOfWorkMock = new Mock<IUnitOfWork>();
            _userService = new UserService(_unitOfWorkMock.Object, _userRepoMock.Object, null, null, null, null);
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
            _userRepoMock = new Mock<IUserRepository>();
            _configurationMock = new Mock<IConfiguration>();
            _emailSenderMock = new Mock<IEmailSender>();
            _userService = new UserService(null, _userRepoMock.Object, _configurationMock.Object, _emailSenderMock.Object, null, null);
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
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var token = "invalid_token";
            user.SetResetPasswordToken("valid_token", DateTime.UtcNow.AddHours(24));

            await Assert.ThrowsAsync<Exception>(() => _userService.ResetPassword(user, "new_password", token));
        }

        [Fact]
        public async Task ResetPassword_ShouldResetPassword_WhenTokenIsValid()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _unitOfWorkMock = new Mock<IUnitOfWork>();
            _userService = new UserService(_unitOfWorkMock.Object, _userRepoMock.Object, null, null, null, null);
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
            _emailSenderMock = new Mock<IEmailSender>();
            _userService = new UserService(null, null, null, _emailSenderMock.Object, null, null);
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
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);
            var dto = new RegisteringPatientDto { Name = "John Doe", PhoneNumber = "123456789", Email = "test@example.com", Password = "Password123!" };
            _userRepoMock.Setup(repo => repo.CheckEmail(dto.Email)).ReturnsAsync(true);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.RegisterPatientAsync(dto));
        }

        [Fact]
        public async Task RegisterPatientAsync_ShouldThrowException_WhenPatientNotFound()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _patientRepoMock = new Mock<IPatientRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, _patientRepoMock.Object, null);
            var dto = new RegisteringPatientDto { Name = "John Doe", PhoneNumber = "123456789", Email = "test@example.com", Password = "Password123!" };

            _userRepoMock.Setup(repo => repo.CheckEmail(dto.Email)).ReturnsAsync(false);
            _patientRepoMock.Setup(repo => repo.GetByNameEmailPhoneAsync(dto.Name, dto.Email, dto.PhoneNumber)).ReturnsAsync((Patient?)null);

            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.RegisterPatientAsync(dto));
        }

        [Fact]
        public async Task ConfirmRegisterPatientAsync_ShouldThrowException_WhenUserNotFound()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);
            var dto = new ConfirmationPatientDto("token", "test@example.com");

            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(DDDSample1.Domain.User.User));

            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.ConfirmRegisterPatientAsync(dto));
        }

        [Fact]
        public async Task ConfirmRegisterPatientAsync_ShouldThrowException_WhenPatientNotFound()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _patientRepoMock = new Mock<IPatientRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, _patientRepoMock.Object, null);
            var dto = new ConfirmationPatientDto("token", "test@example.com");
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var token = "valid_token";
            user.SetConfirmationRegisterPatientToken(token, DateTime.UtcNow.AddHours(24));
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(user);
            _patientRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(Patient));

            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.ConfirmRegisterPatientAsync(dto));
        }

        [Fact]
        public async Task EditPatientAsync_ShouldThrowException_WhenUserNotFound()
        {
            // Arrange
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);
            var dto = new EditingPatientDto { Email = "test@example.com", NameToEdit = "John Doing" };
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("john.doe@example.com", "patient", passwordHasher.HashPassword("password"));
            var patient = new Patient("John Doe", "2024-10-12", "male", "test@example.com", "123456789", "Jane Doe", "jane.doe@example.com", "234532123", "202410000001");
            patient.AssociateUser(user);
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(DDDSample1.Domain.User.User));

            // Act & Assert
            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.EditPatientAsync(dto));
        }

        [Fact]
        public async Task EditPatientAsync_ShouldThrowException_WhenPatientNotFound()
        {
            // Arrange
            _userRepoMock = new Mock<IUserRepository>();
            _patientRepoMock = new Mock<IPatientRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, _patientRepoMock.Object, null);
            var dto = new EditingPatientDto { Email = "test@example.com", NameToEdit = "John Doing" };
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var patient = new Patient("John Doe", "2024-10-12", "male", "john.doe@example.com", "123456789", "Jane Doe", "jane.doe@example.com", "234532123", "202410000001");
            patient.AssociateUser(user);
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(user);
            _patientRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(Patient));

            // Act & Assert
            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.EditPatientAsync(dto));
        }

        [Fact]
        public async Task EditPatientAsync_ShouldThrowException_WhenNewEmailIsNotUnique()
        {
            // Arrange
            _userRepoMock = new Mock<IUserRepository>();
            _patientRepoMock = new Mock<IPatientRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, _patientRepoMock.Object, null);
            var dto = new EditingPatientDto { Email = "testing@example.com", NameToEdit = "John Doing", EmailToEdit = "john.doe@gmail.com"};
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var patient = new Patient("John Doe", "2024-10-12", "male", "test@example.com", "123456789", "Jane Doe", "jane.doe@example.com", "234532123", "202410000001");
            patient.AssociateUser(user);
            var existingPatient = new Patient("Jane Doe", "2024-10-12", "female", "john.doe@example.com", "123456787", "Jane Doe", "jane.doe@example.com", "234532123", "202410000002");
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(user);
            _patientRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(patient);
            _patientRepoMock.Setup(repo => repo.GetByEmailAsync(dto.EmailToEdit)).ReturnsAsync(existingPatient);

            // Act & Assert
            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.EditPatientAsync(dto));
        }

        [Fact]
        public async Task EditPatientAsync_ShouldThrowException_WhenNewPhoneNumberIsNotUnique(){
            // Arrange
            _userRepoMock = new Mock<IUserRepository>();
            _patientRepoMock = new Mock<IPatientRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, _patientRepoMock.Object, null);
            var dto = new EditingPatientDto { Email = "test@example.com", NameToEdit = "John Doing" , PhoneNumberToEdit = "123456789"};
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var patient = new Patient("John Doe", "2024-10-12", "male", "test@example.com", "123456787", "Jane Doe", "jane.doe@example.com", "234532123", "202410000001");
            patient.AssociateUser(user);
            var existingPatient = new Patient("Jane Doe", "2024-10-12", "female", "jane.doe@example.com", "123456789", "Jane Doe", "jane.doe@example.com", "234532123", "202410000002");
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(user);
            _patientRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(patient);
            _patientRepoMock.Setup(repo => repo.GetByPhoneNumberAsync(dto.PhoneNumberToEdit)).ReturnsAsync(existingPatient);

            // Act & Assert
            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.EditPatientAsync(dto));
        }

        [Fact]
        public async Task ConfirmEditPatientSensitiveDataAsync_ShouldThrowException_WhenUserNotFound()
        {
            // Arrange
            _userRepoMock = new Mock<IUserRepository>();
            _patientRepoMock = new Mock<IPatientRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, _patientRepoMock.Object, null);
            var dto = new ConfirmationEditPatientDto("token", "test@example.com", "john.doe@gmail.com", "123456789");
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var patient = new Patient("John Doe", "2024-10-12", "male", "test@example.com", "123456787", "Jane Doe", "jane.doe@example.com", "234532123", "202410000001");
            patient.AssociateUser(user);
            if (dto.Token == null)
            {
                throw new ArgumentNullException(nameof(dto.Token), "Token cannot be null");
            }
            user.SetConfirmationEditPatientToken(dto.Token, DateTime.UtcNow.AddHours(24));
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(DDDSample1.Domain.User.User));

            // Act & Assert
            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.ConfirmEditPatientAsync(dto));
        }

        [Fact]
        public async Task ConfirmEditPatientSensitiveDataAsync_ShouldThrowException_WhenTokenIsInvalid()
        {
            // Arrange
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);

            var dto = new ConfirmationEditPatientDto("token", "jane.doe@example.com", "john.doe@example.com", null);
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var patient = new Patient("John Doe", "2024-10-12", "male", "test@example.com", "123456787", "Jane Doe", "jane.doe@example.com", "234532123", "202410000001");
            patient.AssociateUser(user);
            user.SetConfirmationEditPatientToken("valid_token", DateTime.UtcNow.AddHours(24));
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(user);

            // Act & Assert
            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.ConfirmEditPatientAsync(dto));
        }

        [Fact]
        public async Task ConfirmEditPatientSensitiveDataAsync_ShouldThrowException_WhenPatientNotFound()
        {
            // Arrange
            _userRepoMock = new Mock<IUserRepository>();
            _patientRepoMock = new Mock<IPatientRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, _patientRepoMock.Object, null);

            var dto = new ConfirmationEditPatientDto("token", "test@example.com", "john.doe@gmail.com", null);
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var patient = new Patient("John Doe", "2024-10-12", "male", "test@example.com", "123456787", "Jane Doe", "jane.doe@example.com", "234532123", "202410000001");
            patient.AssociateUser(user);
            user.SetConfirmationEditPatientToken(dto.Token, DateTime.UtcNow.AddHours(24));
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(user);
            _patientRepoMock.Setup(repo => repo.GetByEmailAsync(dto.EmailToEdit)).ReturnsAsync(default(Patient));

            // Act & Assert
            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.ConfirmEditPatientAsync(dto));
        }

        [Fact]
        public async Task DeletePatientAsync_ShouldThrowException_WhenUserNotFound()
        {
            // Arrange
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);

            var dto = new DeletingPatientDto { Email = "john.doe@example.com" };

            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(DDDSample1.Domain.User.User));

            // Act & Assert
            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.DeletePatientAsync(dto));
        }

        [Fact]
        public async Task ConfirmDeletePatientAsync_ShouldThrowException_WhenUserNotFound()
        {
            // Arrange
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);

            var dto = new ConfirmationPatientDto("token", "john.doe@example.com");

            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(DDDSample1.Domain.User.User));

            // Act & Assert
            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.ConfirmDeletePatientAsync(dto));
        }

        [Fact]
        public async Task ConfirmDeletePatientAsync_ShouldThrowException_WhenTokenIsInvalid()
        {
            // Arrange
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);

            var dto = new ConfirmationPatientDto("token", "john.doe@example.com");
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("john.doe@example.com", "patient", passwordHasher.HashPassword("password"));
            user.SetConfirmationDeletePatientToken("valid_token", DateTime.UtcNow.AddHours(24));
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(user);

            // Act & Assert
            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.ConfirmDeletePatientAsync(dto));
        }

        [Fact]
        public async Task ConfirmDeletePatientAsync_ShouldThrowException_WhenPatientNotFound()
        {
            // Arrange
            _userRepoMock = new Mock<IUserRepository>();
            _patientRepoMock = new Mock<IPatientRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, _patientRepoMock.Object, null);

            var dto = new ConfirmationPatientDto("token", "john.doe@example.com");
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("john.doe@example.com", "patient", passwordHasher.HashPassword("password"));
            user.SetConfirmationDeletePatientToken(dto.Token, DateTime.UtcNow.AddHours(24));
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(user);
            _patientRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(Patient));

            // Act & Assert
            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _userService.ConfirmDeletePatientAsync(dto));
        }
    }
}