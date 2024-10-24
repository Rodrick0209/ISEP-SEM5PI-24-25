using System.Runtime.CompilerServices;
using DDDSample1.Controllers;
using DDDSample1.Domain.PatientLoggers;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Http.HttpResults;
using Microsoft.AspNetCore.Mvc;
using Moq;

namespace DDDSample1.Tests.IntegrationTests.Controllers
{
    public class PatientsControllerTestWithIsolation
    {
        private Mock<IPatientRepository>? _patientRepository;
        private Mock<IPatientLoggerRepository>? _patientLoggerRepository;
        private Mock<IEmailSender>? _emailSender;
        private Mock<IUnitOfWork>? _unitOfWork;
        private PatientService? _patientService;
        private PatientsController? _patientsController;

        [Fact]
        public async Task CreateAsync_WithValidDto_ShouldReturnPatientDto()
        {
            _unitOfWork = new Mock<IUnitOfWork>();
            _patientRepository = new Mock<IPatientRepository>();
            _patientLoggerRepository = new Mock<IPatientLoggerRepository>();
            _patientService = new PatientService(_unitOfWork.Object, _patientRepository.Object, _patientLoggerRepository.Object, null);
            _patientsController = new PatientsController(_patientService);

            // Arrange
            var dto = new CreatingPatientProfileDto
            {
                FirstName = "John",
                LastName = "Doe",
                FullName = "John Doe",
                DateOfBirth = "1990-01-01",
                Gender = "male",
                Email = "john.doe@example.com",
                PhoneNumber = "+351 1234567890",
                Street = "123 Main St",
                PostalCode = "12345",
                City = "Anytown",
                Country = "Anycountry",
                EmergencyContactName = "Jane Doe",
                EmergencyContactEmail = "jane.doe@example.com",
                EmergencyContactPhoneNumber = "+351 0987654321"
            };

            _patientRepository.Setup(pr => pr.GetLastPatientRegisteredInMonthAsync()).ReturnsAsync(new Patient("Jane Doe", "1990-01-01", "female", "john.doe2@example.com", "+351 1232567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202410000001"));
            _patientRepository.Setup(pr => pr.AddAsync(It.IsAny<Patient>())).ReturnsAsync(new Patient("John Doe", "1990-01-01", "male", "john.doe@example.com", "+351 1234567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202410000002"));
            _unitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            // Act
            var result = await _patientsController.Create(dto);

            // Assert
            var actionResult = Assert.IsType<ActionResult<PatientDto>>(result);
            var createdAtActionResult = Assert.IsType<CreatedAtActionResult>(actionResult.Result);
            var returnValue = Assert.IsType<PatientDto>(createdAtActionResult.Value);
            Assert.Equal("John Doe", returnValue.FullName);
        }

        [Fact]
        public async Task CreateAsync_WithInvalidDto_ShouldThrowBusinessRuleValidationException()
        {
            _unitOfWork = new Mock<IUnitOfWork>();
            _patientRepository = new Mock<IPatientRepository>();
            _patientService = new PatientService(_unitOfWork.Object, _patientRepository.Object, null, null);
            _patientsController = new PatientsController(_patientService);

            // Arrange
            var dto = new CreatingPatientProfileDto
            {
                FirstName = "John",
                LastName = "Doe",
                FullName = "John Doe",
                DateOfBirth = "1990-01-01",
                Gender = "Male",
                Email = "john.doe@example.com",
                PhoneNumber = "+351 1234567890",
                Street = "123 Main St",
                PostalCode = "12345",
                City = "Anytown",
                Country = "Anycountry",
                EmergencyContactName = "Jane Doe",
                EmergencyContactEmail = "jane.doe@example.com",
                EmergencyContactPhoneNumber = "+351 0987654321"
            };

            _patientRepository.Setup(pr => pr.GetByEmailAsync(dto.Email)).Throws(new BusinessRuleValidationException("Email and/or Phone Number are not unique"));

            // Act
            var result = await _patientsController.Create(dto);

            // Assert
            Assert.NotNull(result);
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }

        [Fact]
        public async Task UpdateAsync_WithValidDto_ShouldReturnPatientDto()
        {
            _unitOfWork = new Mock<IUnitOfWork>();
            _patientRepository = new Mock<IPatientRepository>();
            _patientLoggerRepository = new Mock<IPatientLoggerRepository>();
            _patientService = new PatientService(_unitOfWork.Object, _patientRepository.Object, _patientLoggerRepository.Object, null);
            _patientsController = new PatientsController(_patientService);

            // Arrange
            var dto = new EditingPatientProfileDto
            {
                MedicalRecordNumber = "202410000001",
                FullName = "Jane Doe",

            };

            var patient = new Patient("John Doe", "1990-01-01", "male", "john.doe@example.com", "+351 1234567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202410000001");

            _patientRepository.Setup(pr => pr.GetByMedicalRecordNumberAsync(dto.MedicalRecordNumber)).ReturnsAsync(patient);
            _patientLoggerRepository.Setup(plr => plr.AddAsync(It.IsAny<PatientLogger>())).ReturnsAsync(new PatientLogger(patient.Id, "John Doe", "1990-01-01", "male", "john.doe@example.com", "+351 1234567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202410000001", null, "update", DateTime.Now));
            _unitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            // Act
            var result = await _patientsController.UpdateAsync(dto.MedicalRecordNumber, dto);

            // Assert
            Assert.NotNull(result);
            Assert.IsType<OkObjectResult>(result.Result);
            var actionResult = Assert.IsType<ActionResult<PatientDto>>(result);
        }

        [Fact]
        public async Task UpdateAsync_WithInvalidDto_ShouldThrowBusinessRuleValidationException()
        {
            _unitOfWork = new Mock<IUnitOfWork>();
            _patientRepository = new Mock<IPatientRepository>();
            _patientLoggerRepository = new Mock<IPatientLoggerRepository>();
            _patientService = new PatientService(_unitOfWork.Object, _patientRepository.Object, null, null);
            _patientsController = new PatientsController(_patientService);

            // Arrange
            var dto = new EditingPatientProfileDto
            {
                MedicalRecordNumber = "202410000006",
                FullName = "Jane Doe"
            };

            _patientRepository.Setup(pr => pr.GetByMedicalRecordNumberAsync(dto.MedicalRecordNumber)).Throws(new BusinessRuleValidationException("Patient not found"));

            // Act
            var result = await _patientsController.UpdateAsync(dto.MedicalRecordNumber, dto);

            // Assert
            Assert.NotNull(result);
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }

        [Fact]
        public async Task DeleteAsync_WithValidMedicalRecordNumber_ShouldReturnNoContent()
        {
            _unitOfWork = new Mock<IUnitOfWork>();
            _patientRepository = new Mock<IPatientRepository>();
            _patientLoggerRepository = new Mock<IPatientLoggerRepository>();
            _patientService = new PatientService(_unitOfWork.Object, _patientRepository.Object, _patientLoggerRepository.Object, null);
            _patientsController = new PatientsController(_patientService);

            // Arrange
            var medicalRecordNumber = "202410000001";

            var patient = new Patient("John Doe", "1990-01-01", "male", "john.doe@example.com", "+351 1234567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202410000001");

            _patientRepository.Setup(pr => pr.GetByMedicalRecordNumberAsync(medicalRecordNumber)).ReturnsAsync(patient);
            _patientLoggerRepository.Setup(plr => plr.AddAsync(It.IsAny<PatientLogger>())).ReturnsAsync(new PatientLogger(patient.Id, "John Doe", "1990-01-01", "male", "john.doe@example.com", "+351 1234567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202410000001", null, "delete", DateTime.Now));
            _patientRepository.Setup(pr => pr.Remove(patient));
            _unitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            // Act
            var result = await _patientsController.DeleteAsync(medicalRecordNumber);

            // Assert
            Assert.IsType<NoContentResult>(result);
        }

        [Fact]
        public async Task DeleteAsync_WithInvalidMedicalRecordNumber_ShouldThrowBusinessRuleValidationException()
        {
            _unitOfWork = new Mock<IUnitOfWork>();
            _patientRepository = new Mock<IPatientRepository>();
            _patientService = new PatientService(_unitOfWork.Object, _patientRepository.Object, null, null);
            _patientsController = new PatientsController(_patientService);

            // Arrange
            var medicalRecordNumber = "202410000006";

            _patientRepository.Setup(pr => pr.GetByMedicalRecordNumberAsync(medicalRecordNumber)).Throws(new BusinessRuleValidationException("Patient not found"));

            // Act
            var result = await _patientsController.DeleteAsync(medicalRecordNumber);

            // Assert
            Assert.NotNull(result);
            Assert.IsType<BadRequestObjectResult>(result);
        }

        [Fact]
        public async Task SearchAsync_WithValidDto_ShouldReturnListOfViewPatientDto()
        {
            _unitOfWork = new Mock<IUnitOfWork>();
            _patientRepository = new Mock<IPatientRepository>();
            _patientService = new PatientService(_unitOfWork.Object, _patientRepository.Object, null, null);
            _patientsController = new PatientsController(_patientService);

            // Arrange
            var dto = new SearchFiltersDto
            {
                MedicalRecordNumber = "202410",
                Name = "J",
                DateOfBirth = "1990-01-01",
                Email = ""
            };

            var patients = new List<Patient>
            {
                new Patient("John Doe", "1990-01-01", "male", "john.doe@example.com", "+351 1234567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202410000001"),
                new Patient("Jane Doe", "1990-01-01", "female", "jane.doe@example.com", "+351 0987654321", "123 Main St", "12345", "Anytown", "Anycountry", "John Doe", "john.doe@example.com", "+351 1234567890", "202410000002")
            };

            _patientRepository.Setup(pr => pr.GetByFiltersAsync(dto.MedicalRecordNumber, dto.Name, dto.Email, dto.DateOfBirth)).ReturnsAsync(patients);

            // Act
            var result = await _patientsController.SearchAsync(dto);

            // Assert
            Assert.NotNull(result);
            Assert.NotNull(result.Value);
            Assert.Equal(2, result.Value.Count());
        }
    }
}