using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Patient;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.User;
using DDDSample1.Domain.Utils;
using Moq;
using Xunit;

namespace UnitTest.Domain.Patient
{
    public class PatientServiceTest
    {
        private readonly Mock<IUnitOfWork> _unitOfWorkMock;
        private readonly Mock<IPatientRepository> _patientRepositoryMock;
        private readonly Mock<IEmailSender> _emailSenderMock;
        private readonly PatientService _patientService;

        public PatientServiceTest()
        {
            _unitOfWorkMock = new Mock<IUnitOfWork>();
            _patientRepositoryMock = new Mock<IPatientRepository>();
            _emailSenderMock = new Mock<IEmailSender>();
            _patientService = new PatientService(_unitOfWorkMock.Object, _patientRepositoryMock.Object, _emailSenderMock.Object);
        }

        [Fact]
        public async Task CreateAsync_ShouldCreatePatient_WhenValidData()
        {
            // Arrange
            var dto = new CreatingPatientProfileDto
            {
                FirstName = "John",
                LastName = "Doe",
                FullName = "John Doe",
                DateOfBirth = "1990-01-01",
                Gender = "male",
                Email = "john.doe@example.com",
                PhoneNumber = "1234567890",
                EmergencyContact = "2345678901"
            };


            _patientRepositoryMock.Setup(repo => repo.GetByEmailAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.Patient.Patient));
            _patientRepositoryMock.Setup(repo => repo.GetByPhoneNumberAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.Patient.Patient));

            // Act
            var result = await _patientService.CreateAsync(dto);

            // Assert
            Assert.NotNull(result);
            _patientRepositoryMock.Verify(repo => repo.AddAsync(It.IsAny<DDDSample1.Domain.Patient.Patient>()), Times.Once);
            _unitOfWorkMock.Verify(uow => uow.CommitAsync(), Times.Once);
        }

        [Fact]
        public async Task CreateAsync_ShouldThrowException_WhenEmailNotUnique()
        {
            var dto = new CreatingPatientProfileDto
            {
                FirstName = "John",
                LastName = "Doe",
                FullName = "John Doe",
                DateOfBirth = "1990-01-01",
                Gender = "male",
                Email = "john.doe@example.com",
                PhoneNumber = "1234567890",
                EmergencyContact = "2345678901"
            };

            _patientRepositoryMock.Setup(repo => repo.GetByEmailAsync(It.IsAny<string>())).ReturnsAsync(new DDDSample1.Domain.Patient.Patient(
                new FullName("John Doe"),
                new DateOfBirth(DateTime.Parse("1990-01-01")),
                new Gender("male"),
                new Email("john.doe@example.com"),
                new PhoneNumber("1234567890"),
                new EmergencyContact("Jane Doe"),
                new MedicalRecordNumber("202410000001")
            ));

            // Act & Assert
            await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _patientService.CreateAsync(dto));
        }

        [Fact]
        public async Task UpdateAsync_ShouldUpdatePatientFullName_WhenValidData()
        {
            // Arrange
            var dto = new EditingPatientProfileDto
            {
                MedicalRecordNumber = "202410000001",
                FullName = "John Doe Updated"
            };

            var existingPatient = new DDDSample1.Domain.Patient.Patient(
            new FullName("John Doe"),
            new DateOfBirth(DateTime.Parse("1990-01-01")),
            new Gender("male"),
            new Email("john.doe@example.com"),
            new PhoneNumber("1234567890"),
            new EmergencyContact("Jane Doe"),
            new MedicalRecordNumber("202410000001")
            );

            _patientRepositoryMock.Setup(repo => repo.GetByMedicalRecordNumberAsync(It.IsAny<string>())).ReturnsAsync(existingPatient);

            // Act
            var result = await _patientService.UpdateAsync(dto);

            // Assert
            Assert.NotNull(result);
            Assert.Equal("John Doe Updated", result.FullName);
            _unitOfWorkMock.Verify(uow => uow.CommitAsync(), Times.Once);
        }

        [Fact]
        public async Task UpdateAsync_ShouldUpdatePatientEmail_WhenValidData()
        {
            // Arrange
            var dto = new EditingPatientProfileDto
            {
                MedicalRecordNumber = "202410000001",
                Email = "john.doe.updated@example.com"
            };

            var existingPatient = new DDDSample1.Domain.Patient.Patient(
            new FullName("John Doe"),
            new DateOfBirth(DateTime.Parse("1990-01-01")),
            new Gender("male"),
            new Email("john.doe@example.com"),
            new PhoneNumber("1234567890"),
            new EmergencyContact("Jane Doe"),
            new MedicalRecordNumber("202410000001")
            );

            _patientRepositoryMock.Setup(repo => repo.GetByMedicalRecordNumberAsync(It.IsAny<string>())).ReturnsAsync(existingPatient);
            _patientRepositoryMock.Setup(repo => repo.GetByEmailAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.Patient.Patient));

            // Act
            var result = await _patientService.UpdateAsync(dto);

            // Assert
            Assert.NotNull(result);
            Assert.Equal("john.doe.updated@example.com", result.Email);
            _unitOfWorkMock.Verify(uow => uow.CommitAsync(), Times.Once);
            _emailSenderMock.Verify(sender => sender.SendEmailAsync(It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>()), Times.Once);
        }

        [Fact]
        public async Task UpdateAsync_ShouldUpdatePatientPhoneNumber_WhenValidData()
        {
            // Arrange
            var dto = new EditingPatientProfileDto
            {
                MedicalRecordNumber = "202410000001",
                PhoneNumber = "0987654321"
            };

            var existingPatient = new DDDSample1.Domain.Patient.Patient(
            new FullName("John Doe"),
            new DateOfBirth(DateTime.Parse("1990-01-01")),
            new Gender("male"),
            new Email("john.doe@example.com"),
            new PhoneNumber("1234567890"),
            new EmergencyContact("Jane Doe"),
            new MedicalRecordNumber("202410000001")
            );

            _patientRepositoryMock.Setup(repo => repo.GetByMedicalRecordNumberAsync(It.IsAny<string>())).ReturnsAsync(existingPatient);
            _patientRepositoryMock.Setup(repo => repo.GetByPhoneNumberAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.Patient.Patient));

            // Act
            var result = await _patientService.UpdateAsync(dto);

            // Assert
            Assert.NotNull(result);
            Assert.Equal("0987654321", result.PhoneNumber);
            _unitOfWorkMock.Verify(uow => uow.CommitAsync(), Times.Once);
            _emailSenderMock.Verify(sender => sender.SendEmailAsync(It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>()), Times.Once);
        }

        [Fact]
        public async Task UpdateAsync_ShouldUpdateMedicalConditions_WhenValidData()
        {
            // Arrange
            var dto = new EditingPatientProfileDto
            {
                MedicalRecordNumber = "202410000001",
                MedicalConditions = "Asthma, Diabetes"
            };

            var existingPatient = new DDDSample1.Domain.Patient.Patient(
            new FullName("John Doe"),
            new DateOfBirth(DateTime.Parse("1990-01-01")),
            new Gender("male"),
            new Email("john.doe@example.com"),
            new PhoneNumber("1234567890"),
            new EmergencyContact("Jane Doe"),
            new MedicalRecordNumber("202410000001")
            );

            _patientRepositoryMock.Setup(repo => repo.GetByMedicalRecordNumberAsync(It.IsAny<string>())).ReturnsAsync(existingPatient);
            _patientRepositoryMock.Setup(repo => repo.GetByPhoneNumberAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.Patient.Patient));

            // Act
            var result = await _patientService.UpdateAsync(dto);

            // Assert
            Assert.NotNull(result);
            Assert.Equal("Asthma, Diabetes", result.MedicalConditions);
            _unitOfWorkMock.Verify(uow => uow.CommitAsync(), Times.Once);
        }

        [Fact]
        public async Task UpdateAsync_ShouldUpdatePatient_WhenValidData()
        {
            // Arrange
            var dto = new EditingPatientProfileDto
            {
                MedicalRecordNumber = "202410000001",
                FullName = "John Doe Updated",
                Email = "john.doe.updated@example.com",
                PhoneNumber = "0987654321",
                MedicalConditions = "Asthma"
            };

            var existingPatient = new DDDSample1.Domain.Patient.Patient(
                new FullName("John Doe"),
                new DateOfBirth(DateTime.Parse("1990-01-01")),
                new Gender("male"),
                new Email("john.doe@example.com"),
                new PhoneNumber("1234567890"),
                new EmergencyContact("Jane Doe"),
                new MedicalRecordNumber("202410000001")
            );

            _patientRepositoryMock.Setup(repo => repo.GetByMedicalRecordNumberAsync(It.IsAny<string>())).ReturnsAsync(existingPatient);
            _patientRepositoryMock.Setup(repo => repo.GetByEmailAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.Patient.Patient));
            _patientRepositoryMock.Setup(repo => repo.GetByPhoneNumberAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.Patient.Patient));

            // Act
            var result = await _patientService.UpdateAsync(dto);

            // Assert
            Assert.NotNull(result);
            Assert.Equal("John Doe Updated", result.FullName);
            Assert.Equal("john.doe.updated@example.com", result.Email);
            Assert.Equal("0987654321", result.PhoneNumber);
            Assert.Equal("Asthma", result.MedicalConditions);
            _unitOfWorkMock.Verify(uow => uow.CommitAsync(), Times.Once);
            _emailSenderMock.Verify(sender => sender.SendEmailAsync(It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>()), Times.Once);
        }

        [Fact]
        public async Task GetAllAsync_ShouldReturnAllPatients()
        {
            // Arrange
            var patients = new List<DDDSample1.Domain.Patient.Patient>
            {
                new (
                    new FullName("John Doe"),
                    new DateOfBirth(DateTime.Parse("1990-01-01")),
                    new Gender("male"),
                    new Email("john.doe@example.com"),
                    new PhoneNumber("1234567890"),
                    new EmergencyContact("Jane Doe"),
                    new MedicalRecordNumber("202410000001")
                )
            };

            _patientRepositoryMock.Setup(repo => repo.GetAllAsync()).ReturnsAsync(patients);

            // Act
            var result = await _patientService.GetAllAsync();

            // Assert
            Assert.NotNull(result);
            Assert.Single(result);
        }

        [Fact]
        public async Task GetByMedicalRecordNumberAsync_ShouldReturnPatient_WhenExists()
        {
            // Arrange
            var patient = new DDDSample1.Domain.Patient.Patient(
                new FullName("John Doe"),
                new DateOfBirth(DateTime.Parse("1990-01-01")),
                new Gender("male"),
                new Email("john.doe@example.com"),
                new PhoneNumber("1234567890"),
                new EmergencyContact("Jane Doe"),
                new MedicalRecordNumber("202410000001")
            );

            _patientRepositoryMock.Setup(repo => repo.GetByMedicalRecordNumberAsync(It.IsAny<string>())).ReturnsAsync(patient);

            // Act
            var result = await _patientService.GetByMedicalRecordNumberAsync("202410000001");

            // Assert
            Assert.NotNull(result);
        }

        [Fact]
        public async Task GetByIdAsync_ShouldReturnPatient_WhenExists()
        {
            // Arrange
            var patientId = new PatientId(Guid.NewGuid());
            var patient = new DDDSample1.Domain.Patient.Patient(
                new FullName("John Doe"),
                new DateOfBirth(DateTime.Parse("1990-01-01")),
                new Gender("male"),
                new Email("john.doe@example.com"),
                new PhoneNumber("1234567890"),
                new EmergencyContact("Jane Doe"),
                new MedicalRecordNumber("202410000001")
            );

            _patientRepositoryMock.Setup(repo => repo.GetByIdAsync(It.IsAny<DDDSample1.Domain.Patient.PatientId>())).ReturnsAsync(patient);

            // Act
            var result = await _patientService.GetByIdAsync(patientId);

            // Assert
            Assert.NotNull(result);
        }
    }
}