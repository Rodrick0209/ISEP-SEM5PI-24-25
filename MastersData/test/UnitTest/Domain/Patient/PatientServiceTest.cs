using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.PatientLoggers;
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
        private readonly Mock<IPatientLoggerRepository> _patientLoggerRepositoryMock;
        private readonly Mock<IEmailSender> _emailSenderMock;
        private readonly PatientService _patientService;

        public PatientServiceTest()
        {
            _unitOfWorkMock = new Mock<IUnitOfWork>();
            _patientRepositoryMock = new Mock<IPatientRepository>();
            _patientLoggerRepositoryMock = new Mock<IPatientLoggerRepository>();
            _emailSenderMock = new Mock<IEmailSender>();
            _patientService = new PatientService(_unitOfWorkMock.Object, _patientRepositoryMock.Object, _patientLoggerRepositoryMock.Object, _emailSenderMock.Object);
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


            _patientRepositoryMock.Setup(repo => repo.GetByEmailAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.Patients.Patient));
            _patientRepositoryMock.Setup(repo => repo.GetByPhoneNumberAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.Patients.Patient));

            // Act
            var result = await _patientService.CreateAsync(dto);

            // Assert
            Assert.NotNull(result);
            _patientRepositoryMock.Verify(repo => repo.AddAsync(It.IsAny<DDDSample1.Domain.Patients.Patient>()), Times.Once);
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

            _patientRepositoryMock.Setup(repo => repo.GetByEmailAsync(It.IsAny<string>())).ReturnsAsync(new DDDSample1.Domain.Patients.Patient(
                "John Doe",
                "1990-01-01",
                "male",
                "john.doe@example.com",
                "1234567890",
                "Jane Doe",
                "202410000001"
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

            var existingPatient = new DDDSample1.Domain.Patients.Patient(
                "John Doe",
                "1990-01-01",
                "male",
                "john.doe@example.com",
                "1234567890",
                "Jane Doe",
                "202410000001"
            );

            _patientRepositoryMock.Setup(repo => repo.GetByMedicalRecordNumberAsync(It.IsAny<string>())).ReturnsAsync(existingPatient);
            _patientLoggerRepositoryMock.Setup(repo => repo.AddAsync(It.IsAny<PatientLogger>()));

            // Act
            var result = await _patientService.UpdateAsync(dto);

            // Assert
            Assert.NotNull(result);
            Assert.Equal("John Doe Updated", result.FullName);
            _patientLoggerRepositoryMock.Verify(repo => repo.AddAsync(It.IsAny<PatientLogger>()), Times.Once);
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

            var existingPatient = new DDDSample1.Domain.Patients.Patient(
                "John Doe",
                "1990-01-01",
                "male",
                "john.doe@example.com",
                "1234567890",
                "Jane Doe",
                "202410000001"
            );

            _patientRepositoryMock.Setup(repo => repo.GetByMedicalRecordNumberAsync(It.IsAny<string>())).ReturnsAsync(existingPatient);
            _patientRepositoryMock.Setup(repo => repo.GetByEmailAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.Patients.Patient));

            // Act
            var result = await _patientService.UpdateAsync(dto);

            // Assert
            Assert.NotNull(result);
            Assert.Equal("john.doe.updated@example.com", result.Email);
            _patientLoggerRepositoryMock.Verify(repo => repo.AddAsync(It.IsAny<PatientLogger>()), Times.Once);
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

            var existingPatient = new DDDSample1.Domain.Patients.Patient(
                "John Doe",
                "1990-01-01",
                "male",
                "john.doe@example.com",
                "1234567890",
                "Jane Doe",
                "202410000001"
            );

            _patientRepositoryMock.Setup(repo => repo.GetByMedicalRecordNumberAsync(It.IsAny<string>())).ReturnsAsync(existingPatient);
            _patientRepositoryMock.Setup(repo => repo.GetByPhoneNumberAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.Patients.Patient));

            // Act
            var result = await _patientService.UpdateAsync(dto);

            // Assert
            Assert.NotNull(result);
            Assert.Equal("0987654321", result.PhoneNumber);
            _patientLoggerRepositoryMock.Verify(repo => repo.AddAsync(It.IsAny<PatientLogger>()), Times.Once);
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

            var existingPatient = new DDDSample1.Domain.Patients.Patient(
                "John Doe",
                "1990-01-01",
                "male",
                "john.doe@example.com",
                "1234567890",
                "Jane Doe",
                "202410000001"
            );

            _patientRepositoryMock.Setup(repo => repo.GetByMedicalRecordNumberAsync(It.IsAny<string>())).ReturnsAsync(existingPatient);
            _patientRepositoryMock.Setup(repo => repo.GetByPhoneNumberAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.Patients.Patient));

            // Act
            var result = await _patientService.UpdateAsync(dto);

            // Assert
            Assert.NotNull(result);
            _patientLoggerRepositoryMock.Verify(repo => repo.AddAsync(It.IsAny<PatientLogger>()), Times.Once);
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

            var existingPatient = new DDDSample1.Domain.Patients.Patient(
                "John Doe",
                "1990-01-01",
                "male",
                "john.doe@example.com",
                "1234567890",
                "Jane Doe",
                "202410000001"
            );

            _patientRepositoryMock.Setup(repo => repo.GetByMedicalRecordNumberAsync(It.IsAny<string>())).ReturnsAsync(existingPatient);
            _patientRepositoryMock.Setup(repo => repo.GetByEmailAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.Patients.Patient));
            _patientRepositoryMock.Setup(repo => repo.GetByPhoneNumberAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.Patients.Patient));

            // Act
            var result = await _patientService.UpdateAsync(dto);

            // Assert
            Assert.NotNull(result);
            Assert.Equal("John Doe Updated", result.FullName);
            Assert.Equal("john.doe.updated@example.com", result.Email);
            Assert.Equal("0987654321", result.PhoneNumber);
            Assert.Equal("Asthma", result.MedicalConditions);
            _patientLoggerRepositoryMock.Verify(repo => repo.AddAsync(It.IsAny<PatientLogger>()), Times.Once);
            _unitOfWorkMock.Verify(uow => uow.CommitAsync(), Times.Once);
            _emailSenderMock.Verify(sender => sender.SendEmailAsync(It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>()), Times.Once);
        }

        [Fact]
        public async Task DeleteAsync_ShouldDeletePatient_WhenExists()
        {
            // Arrange
            var dto = new DeletingPatientProfileConfirmationDto
            {
                MedicalRecordNumber = "202410000001",
            };

            var existingPatient = new DDDSample1.Domain.Patients.Patient(
                "John Doe",
                "1990-01-01",
                "male",
                "john.doe@example.com",
                "1234567890",
                "Jane Doe",
                "202410000001"
            );

            _patientRepositoryMock.Setup(repo => repo.GetByMedicalRecordNumberAsync(It.IsAny<string>())).ReturnsAsync(existingPatient);
            _patientRepositoryMock.Setup(repo => repo.GetByEmailAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.Patients.Patient));
            _patientRepositoryMock.Setup(repo => repo.GetByPhoneNumberAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.Patients.Patient));

            // Act
            await _patientService.DeleteAsync(dto);

            // Assert
            _patientRepositoryMock.Verify(repo => repo.Remove(It.IsAny<DDDSample1.Domain.Patients.Patient>()), Times.Once);
            _patientLoggerRepositoryMock.Verify(repo => repo.AddAsync(It.IsAny<PatientLogger>()), Times.Once);
            _unitOfWorkMock.Verify(uow => uow.CommitAsync(), Times.Once);
        }

        [Fact]
        public async Task SeachAsync_ShouldReturnPatients_WhenFiltersAreDisponible()
        {
            // Arrange
            var dto = new SearchFiltersDto
            {
                MedicalRecordNumber = "2024",
                Name = "John",
                Email = "john.",
                DateOfBirth = "1990"
            };

            var patients = new List<DDDSample1.Domain.Patients.Patient>
            {
                new (
                    "John Doe",
                "1990-01-01",
                "male",
                "john.doe@example.com",
                "1234567890",
                "Jane Doe",
                "202410000001"
                ),
                new (
                    "John Doe",
                "1990-01-01",
                "male",
                "john.doe@example.com",
                "1234567891",
                "Jane Doe",
                "202410000002"
                )
            };

            _patientRepositoryMock.Setup(repo => repo.GetAllAsync()).ReturnsAsync(patients);
            _patientRepositoryMock.Setup(repo => repo.GetByFiltersAsync(It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>()))
            .ReturnsAsync(patients.Where(p => p.MedicalRecordNumber._medicalRecordNumber.Contains(dto.MedicalRecordNumber)
                                       && p.FullName.fullName.Contains(dto.Name)
                                       && p.Email.email.Contains(dto.Email)
                                       && p.DateOfBirth.dateOfBirth.ToString("yyyy-MM-dd").Contains(dto.DateOfBirth)).ToList());

            // Act
            var result = await _patientService.SearchAsync(dto);

            // Assert
            Assert.NotNull(result);
            Assert.Equal(2, result.Count);
        }

        [Fact]
        public async Task SeachAsync_ShouldReturnPatient_WhenFiltersAreDisponible()
        {
            // Arrange
            var dto = new SearchFiltersDto
            {
                MedicalRecordNumber = "2024",
                Name = "John Doe",
                Email = "john.",
                DateOfBirth = ""
            };

            var patients = new List<DDDSample1.Domain.Patients.Patient>
            {
                new (
                    "John Doe",
                "1990-01-01",
                "male",
                "john.doe@example.com",
                "1234567890",
                "Jane Doe",
                "202410000001"
                ),
                new (
                    "John Doe",
                "1990-01-01",
                "male",
                "john.doing@example.com",
                "1234567891",
                "Jane Doe",
                "202410000002"
                ),
                new (
                    "John Done",
                    "1990-01-01",
                    "male",
                    "john.done@example.com",
                    "1234567892",
                    "Jane Doe",
                    "202410000009"
                ),
            };

            _patientRepositoryMock.Setup(repo => repo.GetAllAsync()).ReturnsAsync(patients);
            _patientRepositoryMock.Setup(repo => repo.GetByFiltersAsync(It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>()))
                                    .ReturnsAsync(patients.Where(p => p.MedicalRecordNumber._medicalRecordNumber.Contains(dto.MedicalRecordNumber)
                                       && p.FullName.fullName.Contains(dto.Name)
                                       && p.Email.email.Contains(dto.Email)
                                       && p.DateOfBirth.dateOfBirth.ToString("yyyy-MM-dd").Contains(dto.DateOfBirth)).ToList());

            // Act
            var result = await _patientService.SearchAsync(dto);

            // Assert
            Assert.NotNull(result);
            Assert.Single(result);
            Assert.Equal("202410000001", result[0].MedicalRecordNumber);
            Assert.Equal("John Doe", result[0].Name);
            Assert.Equal("john.doe@example.com", result[0].Email);
            Assert.Equal("1990-01-01", result[0].DateOfBirth);
        }

        [Fact]
        public async Task SeachAsync_ShouldReturnAllPatients_WhenFiltersAreNotDisponible()
        {
            // Arrange
            var dto = new SearchFiltersDto
            {
                MedicalRecordNumber = "",
                Name = "",
                Email = "",
                DateOfBirth = ""
            };

             var patients = new List<DDDSample1.Domain.Patients.Patient>
            {
                new (
                    "John Doe",
                "1990-01-01",
                "male",
                "john.doe@example.com",
                "1234567890",
                "Jane Doe",
                "202410000001"
                ),
                new (
                    "John Doe",
                "1990-01-01",
                "male",
                "john.doe@example.com",
                "1234567891",
                "Jane Doe",
                "202410000002"
                ),
                new (
                    "John Done",
                    "1990-01-01",
                    "male",
                    "john.done@example.com",
                    "1234567892",
                    "Jane Doe",
                    "202410000009"
                ),
            };

            _patientRepositoryMock.Setup(repo => repo.GetAllAsync()).ReturnsAsync(patients);
            _patientRepositoryMock.Setup(repo => repo.GetByFiltersAsync(It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>()))
                                    .ReturnsAsync(patients.Where(p => p.MedicalRecordNumber._medicalRecordNumber.Contains(dto.MedicalRecordNumber)
                                       && p.FullName.fullName.Contains(dto.Name)
                                       && p.Email.email.Contains(dto.Email)
                                       && p.DateOfBirth.dateOfBirth.ToString("yyyy-MM-dd").Contains(dto.DateOfBirth)).ToList());
            // Act
            var result = await _patientService.SearchAsync(dto);

            // Assert
            Assert.NotNull(result);
            Assert.Equal(3, result.Count);
        }



        [Fact]
        public async Task GetAllAsync_ShouldReturnAllPatients()
        {
            // Arrange
            var patients = new List<DDDSample1.Domain.Patients.Patient>
            {
                new (
                    "John Doe",
                "1990-01-01",
                "male",
                "john.doe@example.com",
                "1234567890",
                "Jane Doe",
                "202410000001"
                ),
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
            var patient = new DDDSample1.Domain.Patients.Patient(
                "John Doe",
                "1990-01-01",
                "male",
                "john.doe@example.com",
                "1234567890",
                "Jane Doe",
                "202410000001"
            );

            _patientRepositoryMock.Setup(repo => repo.GetByMedicalRecordNumberAsync(It.IsAny<string>())).ReturnsAsync(patient);

            // Act
            var result = await _patientService.GetByMedicalRecordNumberAsync("202410000001");

            // Assert
            Assert.NotNull(result);
        }

    }
}

