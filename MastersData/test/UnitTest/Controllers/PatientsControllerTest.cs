using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Controllers;
using DDDSample1.Domain.PatientLoggers;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Mvc;
using Moq;
using Xunit;

namespace DDDSample1.Tests.UnitTests.Controllers
{
    public class PatientsControllerTest
    {
        private Mock<IPatientService>? _mockService;
        private PatientsController? _controller;

        [Fact]
        public async Task Create_ReturnsCreatedAtActionResult_WhenPatientIsCreated()
        {
            _mockService = new Mock<IPatientService>();
            _controller = new PatientsController(_mockService.Object);

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
            var patientDto = new PatientDto(
                Guid.NewGuid(),
                "John",
                "Doe",
                "John Doe",
                "1990-01-01",
                "Male",
                "john.doe@example.com",
                new AddressDto("123 Main St", "12345", "Anytown", "Anycountry"),
                new EmergencyContactDto("Jane Doe", "jane.doe@example.com", "+351 0987654321"),
                null
            );
            _mockService.Setup(service => service.CreateAsync(dto)).ReturnsAsync(patientDto);

            // Act
            var result = await _controller.CreateAsync(dto);
            
            // Assert
            var actionResult = Assert.IsType<CreatedAtActionResult>(result.Result);
            Assert.Equal(nameof(_controller.GetGetById), actionResult.ActionName);
            Assert.Equal(patientDto, actionResult.Value);
        }

        [Fact]
        public async Task Create_ReturnsBadRequest_WhenExceptionIsThrown()
        {
            _mockService = new Mock<IPatientService>();
            _controller = new PatientsController(_mockService.Object);

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
            _mockService.Setup(service => service.CreateAsync(dto)).ThrowsAsync(new Exception("Error"));

            // Act
            var result = await _controller.CreateAsync(dto);

            // Assert
            var actionResult = Assert.IsType<BadRequestObjectResult>(result.Result);
            var value = actionResult.Value;
            Assert.NotNull(value);
            Assert.Equal("Error", value.GetType().GetProperty("Message")?.GetValue(value));
        }

        [Fact]
        public async Task UpdateAsync_ReturnsOkResult_WhenPatientIsUpdated()
        {
            _mockService = new Mock<IPatientService>();
            _controller = new PatientsController(_mockService.Object);

            // Arrange
            var dto = new EditingPatientProfileDto { MedicalRecordNumber = "123" };
            var patientDto = new PatientDto(
                Guid.NewGuid(),
                "John",
                "Doe",
                "John Doe",
                "1990-01-01",
                "Male",
                "john.doe@example.com",
                new AddressDto("123 Main St", "12345", "Anytown", "Anycountry"),
                new EmergencyContactDto("Jane Doe", "jane.doe@example.com", "+351 0987654321"),
                null
            );
            _mockService.Setup(service => service.UpdateAsync(dto)).ReturnsAsync(patientDto);

            // Act
            var result = await _controller.UpdateAsync("123", dto);

            // Assert
            var actionResult = Assert.IsType<OkObjectResult>(result.Result);
            Assert.Equal(patientDto, actionResult.Value);
        }

        [Fact]
        public async Task UpdateAsync_ReturnsBadRequest_WhenMedicalRecordNumberDoesNotMatch()
        {
            _mockService = new Mock<IPatientService>();
            _controller = new PatientsController(_mockService.Object);
            
            // Arrange
            var dto = new EditingPatientProfileDto { MedicalRecordNumber = "123" };

            // Act
            var result = await _controller.UpdateAsync("456", dto);

            // Assert
            Assert.IsType<BadRequestResult>(result.Result);
        }

        [Fact]
        public async Task DeleteAsync_ReturnsNoContentResult_WhenPatientIsDeleted()
        {
            _mockService = new Mock<IPatientService>();
            _controller = new PatientsController(_mockService.Object);

            // Arrange
            var dto = new DeletingPatientProfileConfirmationDto { MedicalRecordNumber = "123" };
            _mockService.Setup(service => service.DeleteAsync("123")).Returns(Task.CompletedTask);

            // Act
            var result = await _controller.DeleteAsync("123");

            // Assert
            Assert.IsType<NoContentResult>(result);
        }

        [Fact]
        public async Task GetAllAsync_ReturnsAllPatients()
        {
            _mockService = new Mock<IPatientService>();
            _controller = new PatientsController(_mockService.Object);

            // Arrange
            var patients = new List<PatientDto> { new PatientDto(), new PatientDto() };
            _mockService.Setup(service => service.GetAllAsync()).ReturnsAsync(patients);

            // Act
            var result = await _controller.GetAllAsync();

            // Assert
            var actionResult = Assert.IsType<ActionResult<IEnumerable<PatientDto>>>(result);
            Assert.Equal(patients, actionResult.Value);
        }

        [Fact]
        public async Task GetByMedicalRecordNumberAsync_ReturnsPatient_WhenPatientExists()
        {
            _mockService = new Mock<IPatientService>();
            _controller = new PatientsController(_mockService.Object);

            // Arrange
            var patientDto = new PatientDto();
            _mockService.Setup(service => service.GetByMedicalRecordNumberAsync("123")).ReturnsAsync(patientDto);


            // Act
            var result = await _controller.GetByMedicalRecordNumberAsync("123");

            var actionResult = Assert.IsType<ActionResult<PatientDto>>(result);
            Assert.Equal(patientDto, actionResult.Value);
        }

        [Fact]
        public async Task GetByMedicalRecordNumberAsync_ReturnsNotFound_WhenPatientDoesNotExist()
        {
            _mockService = new Mock<IPatientService>();
            _controller = new PatientsController(_mockService.Object);

            // Arrange
            _mockService.Setup(service => service.GetByMedicalRecordNumberAsync("123")).ReturnsAsync((PatientDto?)null);

            // Act
            var result = await _controller.GetByMedicalRecordNumberAsync("123");

            // Assert
            Assert.IsType<NotFoundResult>(result.Result);
        }

        [Fact]
        public async Task GetGetById_ReturnsPatient_WhenPatientExists()
        {
            _mockService = new Mock<IPatientService>();
            _controller = new PatientsController(_mockService.Object);

            // Arrange
            var patientDto = new PatientDto();
            _mockService.Setup(service => service.GetByIdAsync(It.IsAny<PatientId>())).ReturnsAsync(patientDto);

            // Act
            var result = await _controller.GetGetById(Guid.NewGuid());

            // Assert
            var actionResult = Assert.IsType<ActionResult<PatientDto>>(result);
            Assert.Equal(patientDto, actionResult.Value);
            
        }

        [Fact]
        public async Task GetGetById_ReturnsNotFound_WhenPatientDoesNotExist()
        {
            _mockService = new Mock<IPatientService>();
            _controller = new PatientsController(_mockService.Object);

            // Arrange
            _mockService.Setup(service => service.GetByIdAsync(It.IsAny<PatientId>())).ReturnsAsync((PatientDto?)null);

            // Act
            var result = await _controller.GetGetById(Guid.NewGuid());

            // Assert
            Assert.IsType<NotFoundResult>(result.Result);
        }

        [Fact]
        public async Task SearchAsync_ReturnsPatients_WhenPatientsMatchSearchCriteria()
        {
            _mockService = new Mock<IPatientService>();
            _controller = new PatientsController(_mockService.Object);
            
            // Arrange
            var dto = new SearchFiltersDto();
            var patients = new List<ViewPatientDto>();
            _mockService.Setup(service => service.SearchAsync(dto)).ReturnsAsync(patients);

            // Act
            var result = await _controller.SearchAsync(dto);

            // Assert
            var actionResult = Assert.IsType<ActionResult<IEnumerable<ViewPatientDto>>>(result);
            Assert.Empty(actionResult.Value);
        }
    }
}