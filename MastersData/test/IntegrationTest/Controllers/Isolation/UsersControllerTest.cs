using System.Xml;
using DDDSample1.Controllers;
using DDDSample1.Domain.PatientLoggers;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Http.HttpResults;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using Moq;

namespace DDDSample1.Tests.IntegrationTests.Controllers
{
    public class UsersControllerTest
    {
        private Mock<IUnitOfWork>? _unitOfWork;
        private Mock<IUserRepository>? _userRepoMock;
        private Mock<IPatientRepository>? _patientRepoMock;
        private Mock<IPatientLoggerRepository>? _patientLoggerRepository;
        private Mock<IEmailSender>? _emailSender;
        private Mock<IConfiguration>? _configuration;
        private UserService? _userService;
        private UsersController? _controller;

         [Fact]
        public async Task RegisterPatientAsync_BadRequest_WhenEmailIsNotUnique()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);
            _controller = new UsersController(_userService);

            // Arrange
            var dto = new RegisteringPatientDto { Name = "John Doe", Email = "test@example.com", PhoneNumber = "123456789", Password = "Password123!"};
            _userRepoMock.Setup(repo => repo.CheckEmail(dto.Email)).ReturnsAsync(true);

            // Act
            var result = await _controller.RegisterPatientAsync(dto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }

        [Fact]
        public async Task RegisterPatientAsync_BadRequest_WhenPatientNotFound()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _patientRepoMock = new Mock<IPatientRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, _patientRepoMock.Object, null);
            _controller = new UsersController(_userService);

            // Arrange
            var dto = new RegisteringPatientDto { Name = "John Doe", Email = "test@example.com", PhoneNumber = "123456789", Password = "Password123!"};

            _userRepoMock.Setup(repo => repo.CheckEmail(dto.Email)).ReturnsAsync(false);
            _patientRepoMock.Setup(repo => repo.GetByNameEmailPhoneAsync(dto.Name, dto.Email, dto.PhoneNumber)).ReturnsAsync((Patient?)null);

            // Act
            var result = await _controller.RegisterPatientAsync(dto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }

        [Fact]
        public async Task ConfirmRegisterPatientAsync_BadRequest_WhenUserNotFound()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);
            _controller = new UsersController(_userService);

            // Arrange
            var dto = new ConfirmationPatientDto("token", "test@example.com");

            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(DDDSample1.Domain.User.User));

            // Act
            var result = await _controller.ConfirmRegisterPatientAsync(dto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }

        [Fact]
        public async Task ConfirmRegisterPatientAsync_NotFound_WhenPatientNotFound()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _patientRepoMock = new Mock<IPatientRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, _patientRepoMock.Object, null);
            _controller = new UsersController(_userService);

            // Arrange
            var dto = new ConfirmationPatientDto("token", "test@example.com");
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var token = "valid_token";
            user.SetConfirmationRegisterPatientToken(token, DateTime.UtcNow.AddHours(24));
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(user);
            _patientRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(Patient));

            // Act
            var result = await _controller.ConfirmRegisterPatientAsync(dto);

            // Assert
            Assert.IsType<NotFoundResult>(result);
        }

        [Fact]
        public async Task EditPatientAsync_BadRequest_WhenUserNotFound()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);
            _controller = new UsersController(_userService);

            // Arrange
            var dto = new EditingPatientDto { Email = "test@example.com", NameToEdit = "John Doing" };
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("john.doe@example.com", "patient", passwordHasher.HashPassword("password"));
            var patient = new Patient("John Doe", "2024-10-12", "male", "test@example.com", "+351 123456789", "Main Street", "12345", "City", "Country", "Jane Doe", "jane.doe@example.com", "+351 234532123", "202410000001");
            patient.AssociateUser(user);
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(DDDSample1.Domain.User.User));

            // Act
            var result = await _controller.EditPatientAsync(dto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }

        [Fact]
        public async Task EditPatientAsync_BadRequest_WhenPatientNotFound()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _patientRepoMock = new Mock<IPatientRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, _patientRepoMock.Object, null);
            _controller = new UsersController(_userService);

            // Arrange
            var dto = new EditingPatientDto { Email = "test@example.com", NameToEdit = "John Doing" };
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var patient = new Patient("John Doe", "2024-10-12", "male", "test@example.com", "+351 123456789", "Main Street", "12345", "City", "Country", "Jane Doe", "jane.doe@example.com", "+351 234532123", "202410000001");
            patient.AssociateUser(user);
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(user);
            _patientRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(Patient));

            // Act
            var result = await _controller.EditPatientAsync(dto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }

        [Fact]
        public async Task EditPatientAsync_BadRequest_WhenNewEmailIsNotUnique()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _patientRepoMock = new Mock<IPatientRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, _patientRepoMock.Object, null);
            _controller = new UsersController(_userService);

            // Arrange
            var dto = new EditingPatientDto { Email = "testing@example.com", NameToEdit = "John Doing", EmailToEdit = "john.doe@gmail.com"};
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var patient = new Patient("John Doe", "2024-10-12", "male", "test@example.com", "+351 123456789", "Main Street", "12345", "City", "Country", "Jane Doe", "jane.doe@example.com", "+351 234532123", "202410000001");
            patient.AssociateUser(user);
            var existingPatient = new Patient("John Doe", "2024-10-12", "male", "test1@example.com", "+351 123456786", "Main Street", "12345", "City", "Country", "Jane Doe", "jane.doe@example.com", "+351 234532123", "202410000002");
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(user);
            _patientRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(patient);
            _patientRepoMock.Setup(repo => repo.GetByEmailAsync(dto.EmailToEdit)).ReturnsAsync(existingPatient);

            // Act
            var result = await _controller.EditPatientAsync(dto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }

        [Fact]
        public async Task EditPatientAsync_BadRequest_WhenNewPhoneNumberIsNotUnique(){
            _userRepoMock = new Mock<IUserRepository>();
            _patientRepoMock = new Mock<IPatientRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, _patientRepoMock.Object, null);
            _controller = new UsersController(_userService);

            // Arrange
            var dto = new EditingPatientDto { Email = "test@example.com", NameToEdit = "John Doing" , PhoneNumberToEdit = "123456789"};
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var patient = new Patient("John Doe", "2024-10-12", "male", "test@example.com", "+351 123456789", "Main Street", "12345", "City", "Country", "Jane Doe", "jane.doe@example.com", "+351 234532123", "202410000001");
            patient.AssociateUser(user);
            var existingPatient = new Patient("John Doe", "2024-10-12", "male", "test1@example.com", "+351 123456786", "Main Street", "12345", "City", "Country", "Jane Doe", "jane.doe@example.com", "+351 234532123", "202410000002");
            _patientRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(patient);
            _patientRepoMock.Setup(repo => repo.GetByPhoneNumberAsync(dto.PhoneNumberToEdit)).ReturnsAsync(existingPatient);

            // Act
            var result = await _controller.EditPatientAsync(dto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }

        [Fact]
        public async Task ConfirmEditPatientSensitiveDataAsync_BadRequestWhenUserNotFound()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _patientRepoMock = new Mock<IPatientRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, _patientRepoMock.Object, null);
            _controller = new UsersController(_userService);

             // Arrange
            var dto = new ConfirmationEditPatientDto("token", "test@example.com", "john.doe@gmail.com", "123456789");
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var patient = new Patient("John Doe", "2024-10-12", "male", "test@example.com", "+351 123456789", "Main Street", "12345", "City", "Country", "Jane Doe", "jane.doe@example.com", "+351 234532123", "202410000001");
            patient.AssociateUser(user);
            if (dto.Token == null)
            {
                throw new ArgumentNullException(nameof(dto.Token), "Token cannot be null");
            }
            user.SetConfirmationEditPatientToken(dto.Token, DateTime.UtcNow.AddHours(24));
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(DDDSample1.Domain.User.User));

            // Act
            var result = await _controller.ConfirmEditPatientAsync(dto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }

        [Fact]
        public async Task ConfirmEditPatientSensitiveDataAsync_BadRequest_WhenTokenIsInvalid()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);
            _controller = new UsersController(_userService);

            // Arrange
            var dto = new ConfirmationEditPatientDto("token", "jane.doe@example.com", "john.doe@example.com", null);
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var patient = new Patient("John Doe", "2024-10-12", "male", "test@example.com", "+351 123456789", "Main Street", "12345", "City", "Country", "Jane Doe", "jane.doe@example.com", "+351 234532123", "202410000001");
            patient.AssociateUser(user);
            user.SetConfirmationEditPatientToken("valid_token", DateTime.UtcNow.AddHours(24));
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(user);

            // Act
            var result = await _controller.ConfirmEditPatientAsync(dto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }

        [Fact]
        public async Task ConfirmEditPatientSensitiveDataAsync_BadRequest_WhenPatientNotFound()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _patientRepoMock = new Mock<IPatientRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, _patientRepoMock.Object, null);
            _controller = new UsersController(_userService);

            // Arrange
            var dto = new ConfirmationEditPatientDto("token", "test@example.com", "john.doe@gmail.com", null);
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("test@example.com", "patient", passwordHasher.HashPassword("password"));
            var patient = new Patient("John Doe", "2024-10-12", "male", "test@example.com", "+351 123456789", "Main Street", "12345", "City", "Country", "Jane Doe", "jane.doe@example.com", "+351 234532123", "202410000001");
            patient.AssociateUser(user);
            user.SetConfirmationEditPatientToken(dto.Token, DateTime.UtcNow.AddHours(24));
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(user);
            _patientRepoMock.Setup(repo => repo.GetByEmailAsync(dto.EmailToEdit)).ReturnsAsync(default(Patient));

            // Act
            var result = await _controller.ConfirmEditPatientAsync(dto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }

        [Fact]
        public async Task DeletePatientAsync_BadRequest_WhenUserNotFound()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);
            _controller = new UsersController(_userService);

            // Arrange
            string email = "john.doe@example.com";

            _userRepoMock.Setup(repo => repo.GetByEmailAsync(email)).ReturnsAsync(default(DDDSample1.Domain.User.User));

            // Act
            var result = await _controller.DeletePatientAsync(email);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }

        [Fact]
        public async Task ConfirmDeletePatientAsync_BadRequest_WhenUserNotFound()
        {
            // Arrange
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);
            _controller = new UsersController(_userService);

            var dto = new ConfirmationPatientDto("token", "john.doe@example.com");

            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(DDDSample1.Domain.User.User));

            // Act
            var result = await _controller.ConfirmDeletePatientAsync(dto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }

        [Fact]
        public async Task ConfirmDeletePatientAsync_BadRequest_WhenTokenIsInvalid()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, null, null);
            _controller = new UsersController(_userService);

            // Arrange
            var dto = new ConfirmationPatientDto("token", "john.doe@example.com");
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("john.doe@example.com", "patient", passwordHasher.HashPassword("password"));
            user.SetConfirmationDeletePatientToken("valid_token", DateTime.UtcNow.AddHours(24));
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(user);

            // Act
            var result = await _controller.ConfirmDeletePatientAsync(dto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }

        [Fact]
        public async Task ConfirmDeletePatientAsync_BadRequest_WhenPatientNotFound()
        {
            _userRepoMock = new Mock<IUserRepository>();
            _patientRepoMock = new Mock<IPatientRepository>();
            _userService = new UserService(null, _userRepoMock.Object, null, null, _patientRepoMock.Object, null);
            _controller = new UsersController(_userService);

            // Arrange
            var dto = new ConfirmationPatientDto("token", "john.doe@example.com");
            PasswordHasher passwordHasher = new PasswordHasher();
            var user = new DDDSample1.Domain.User.User ("john.doe@example.com", "patient", passwordHasher.HashPassword("password"));
            user.SetConfirmationDeletePatientToken(dto.Token, DateTime.UtcNow.AddHours(24));
            _userRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(user);
            _patientRepoMock.Setup(repo => repo.GetByEmailAsync(dto.Email)).ReturnsAsync(default(Patient));

            // Act
            var result = await _controller.ConfirmDeletePatientAsync(dto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }
    }
}