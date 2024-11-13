using System;
using System.Security.Claims;
using System.Threading.Tasks;
using DDDSample1.Controllers;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.OperationRequestLoggers;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.PatientLoggers;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.Utils;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Moq;
using Xunit;

namespace DDDSample1.Tests.IntegrationTests.Controllers
{

    public class OperationRequestControllerTests
    {

        private Mock<IUnitOfWork> _unitOfWork;
        private Mock<IOperationRequestRepository> _operationRequestRepository;
        private Mock<IOperationRequestLoggerRepository> _operationRequestLoggerRepository;
        private Mock<IPatientRepository> _patientRepository;
        private Mock<IPatientLoggerRepository> _patientLoggerRepository;
        private Mock<IStaffRepository> _staffRepository;
        private Mock<IOperationTypeRepository> _operationTypeRepository;
        private OperationRequestService _operationRequestService;
        private OperationRequestController _operationRequestController;

        public OperationRequestControllerTests()
        {
            // Inicializar mocks dos repositórios
            _unitOfWork = new Mock<IUnitOfWork>();
            _operationRequestRepository = new Mock<IOperationRequestRepository>();
            _operationRequestLoggerRepository = new Mock<IOperationRequestLoggerRepository>();
            _patientRepository = new Mock<IPatientRepository>();
            _patientLoggerRepository = new Mock<IPatientLoggerRepository>();
            _staffRepository = new Mock<IStaffRepository>();
            _operationTypeRepository = new Mock<IOperationTypeRepository>();

            // Instanciar o serviço com mocks
            _operationRequestService = new OperationRequestService(
                _unitOfWork.Object,
                _operationRequestRepository.Object,
                _operationTypeRepository.Object,
                _staffRepository.Object,
                _patientRepository.Object,
                _operationRequestLoggerRepository.Object
            );

            // Instanciar o controlador com o serviço real
            _operationRequestController = new OperationRequestController(_operationRequestService);


            var user = new ClaimsPrincipal(new ClaimsIdentity(new Claim[]
            {
            new Claim(ClaimTypes.Email, "D202512345@gmail.com"),
            }, "mock"));

            _operationRequestController.ControllerContext = new ControllerContext
            {
                HttpContext = new DefaultHttpContext { User = user }
            };


        }

        [Fact]
        public async Task CreateAsync_WithValidDto_ReturnsCreatedResult()
        {
            // Definir IDs fixos para entidades relacionadas
            var operationTypeId = Guid.Parse("11111111-1111-1111-1111-111111111111");
            var patientId = Guid.Parse("22222222-2222-2222-2222-222222222222");
            var staffId = "D202512345";

            // Create required staff
            var specialization1 = new Specialization("Ortopedia");


            var requiredStaff1 = new RequiredStaff(10, specialization1.Id);
            var requiredStaff2 = new RequiredStaff(20, specialization1.Id);
            var requiredStaffList1 = new List<RequiredStaff> { requiredStaff1, requiredStaff2 };
            var requiredStaffList2 = new List<RequiredStaff> { new RequiredStaff(20, specialization1.Id) };
            var requiredStaffList3 = new List<RequiredStaff> { new RequiredStaff(2, specialization1.Id) };

            var phase1 = new Phase(20, requiredStaffList1);
            var phase2 = new Phase(90, requiredStaffList2);
            var phase3 = new Phase(15, requiredStaffList3);



            var availabilitySlot1 = new AvailabilitySlot(staffId);
            availabilitySlot1.AddAvailability(DateOnly.Parse("2025-01-01"), 610, 720);


            // Configurar mocks para retornar entidades com esses IDs fixos
            _operationTypeRepository.Setup(repo => repo.GetByIdAsync(new OperationTypeId(operationTypeId)))
                .ReturnsAsync(new OperationType("New Operation Type", true, phase1, phase2, phase3, specialization1.Id));

            _patientRepository.Setup(repo => repo.GetByIdAsync(new PatientId(patientId)))
                .ReturnsAsync(new Patient("Jane Doe", "1990-01-01", "female", "john.doe2@example.com", "+351 1232567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202410000001"));

            _staffRepository.Setup(repo => repo.GetByIdAsync(new StaffId(staffId)))
                .ReturnsAsync(new Staff(new StaffId("D202512345"), "Miguel", "12345", specialization1.Id.AsString(), availabilitySlot1.Id.AsString(), "mamarNoC@gmail.com", "+351123456789", "Doctor","True"));

            _unitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            // Criar DTO com IDs válidos para o teste
            var dto = new OperationRequestDto
            (
                "2025-01-01",
                "eletric",
                patientId.ToString(),
                operationTypeId.ToString(),
                staffId.ToString()
            );


            // Act
            var result = await _operationRequestController.Create(dto);

            var actionResult = Assert.IsType<ActionResult<OperationRequestDto>>(result);
            var createdAtActionResult = Assert.IsType<CreatedAtActionResult>(actionResult.Result);
            var returnValue = Assert.IsType<OperationRequestDto>(createdAtActionResult.Value);

            // Assert
            Assert.Equal(operationTypeId.ToString(), returnValue.OperationTypeId);
            Assert.Equal(patientId.ToString(), returnValue.PatientId);
            Assert.Equal(staffId.ToString(), returnValue.DoctorThatWillPerformId);


        }




        [Fact]
        public async Task CreateAsync_WithInvalidDto_ReturnsBadRequest()
        {
            // Definir IDs fixos para entidades relacionadas
            var operationTypeId = Guid.Parse("11111111-1111-1111-1111-111111111111");
            var patientId = Guid.Parse("22222222-2222-2222-2222-222222222222");
            var staffId = "D202512345";

            // Configurar mocks para retornar valores nulos ou inválidos
            _operationTypeRepository.Setup(repo => repo.GetByIdAsync(new OperationTypeId(operationTypeId)))
                .ReturnsAsync((OperationType)null); // Retorna null para simular que o tipo de operação não foi encontrado

            _patientRepository.Setup(repo => repo.GetByIdAsync(new PatientId(patientId)))
                .ReturnsAsync((Patient)null); // Retorna null para simular que o paciente não foi encontrado

            _staffRepository.Setup(repo => repo.GetByIdAsync(new StaffId(staffId)))
                .ReturnsAsync((Staff)null); // Retorna null para simular que o médico não foi encontrado

            _unitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            // Criar DTO com valores inválidos para o teste
            var dto = new OperationRequestDto
            (
                "2025-01-01",
                "invalid_priority", // Prioridade inválida
                patientId.ToString(),
                operationTypeId.ToString(),
                staffId.ToString()
            );

            // Act
            var result = await _operationRequestController.Create(dto);

            // Assert
            var actionResult = Assert.IsType<ActionResult<OperationRequestDto>>(result);
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(actionResult.Result);

        }






        [Fact]
        public async Task PutAsync_WithValidDto_ReturnsSuccess()
        {
            // Arrange
            var doctorEmail = "D202512345@gmail.com";
            var doctorId = new Email(doctorEmail).getFirstPartOfEmail();


            var operationRequest = new OperationRequest(
                "2025-01-01",
                "eletric",
                "patientId",
                "operationTypeId",
                doctorId,
                "doctorThatWillPerformId"
            );

            var changeOperationRequestDto = new ChangeOperationRequestDto(
                operationRequest.Id.AsGuid(),
                "2029-01-01",
                "urgency"
            );

            _operationRequestRepository.Setup(repo => repo.GetByIdAsync(operationRequest.Id))
                .ReturnsAsync(operationRequest);

            _operationRequestLoggerRepository.Setup(repo => repo.AddAsync(It.IsAny<OperationRequestLogger>()))
                    .Returns(Task.FromResult(new OperationRequestLogger("2025-01-01", "eletric", "operationTypeId", "doctorThatWillPerformId", "doctorThatRequestedId", operationRequest.Id.ToString(), "Update")));

            _unitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            // Act
            var result = await _operationRequestController.Update(operationRequest.Id.AsGuid(), changeOperationRequestDto);

            // Assert
            var actionResult = Assert.IsType<ActionResult<OperationRequestDto>>(result);
            var okResult = Assert.IsType<OkObjectResult>(actionResult.Result);
            var returnValue = Assert.IsType<OperationRequestDto>(okResult.Value);

            Assert.Equal(changeOperationRequestDto.Id, returnValue.Id);
            Assert.Equal(changeOperationRequestDto.DeadLineDate, returnValue.DeadLineDate);
            Assert.Equal(changeOperationRequestDto.Priority, returnValue.Priority);
        }



        [Fact]
        public async Task PutAsync_WithInValidDto_ReturnsUnSuccess()
        {
            // Arrange
            var doctorEmail = "D202512345@gmail.com";
            var doctorId = new Email(doctorEmail).getFirstPartOfEmail();


            var operationRequest = new OperationRequest(
                "2025-01-01",
                "eletric",
                "patientId",
                "operationTypeId",
                doctorId,
                "doctorThatWillPerformId"
            );

            var changeOperationRequestDto = new ChangeOperationRequestDto(
                operationRequest.Id.AsGuid(),
                "2029-01-01",
                "error"
            );

            _operationRequestRepository.Setup(repo => repo.GetByIdAsync(operationRequest.Id))
                .ReturnsAsync(operationRequest);

            _operationRequestLoggerRepository.Setup(repo => repo.AddAsync(It.IsAny<OperationRequestLogger>()))
                    .Returns(Task.FromResult(new OperationRequestLogger("2025-01-01", "eletric", "operationTypeId", "doctorThatWillPerformId", "doctorThatRequestedId", operationRequest.Id.ToString(), "Update")));

            _unitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            // Act
            var result = await _operationRequestController.Update(operationRequest.Id.AsGuid(), changeOperationRequestDto);

            // Assert
            var actionResult = Assert.IsType<ActionResult<OperationRequestDto>>(result);
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(actionResult.Result);
        }


        [Fact]
        public async Task DeleteAsync_WithValidId_ReturnsSuccess()
        {
            // Arrange
            var operationRequest = new OperationRequest(
                    "2025-01-01",
                    "eletric",
                    "patientId",
                    "operationTypeId",
                    "doctorThatRequestedId",
                    "doctorThatWillPerformId"
            );

            _operationRequestRepository.Setup(repo => repo.GetByIdAsync(operationRequest.Id))
                    .ReturnsAsync(operationRequest);

            _operationRequestRepository.Setup(pr => pr.Remove(It.IsAny<OperationRequest>()));

            _unitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);


            // Act
            var result = await _operationRequestController.Delete(operationRequest.Id.AsGuid());

            // Assert
            var actionResult = Assert.IsType<ActionResult<OperationRequestDto>>(result);
            var okResult = Assert.IsType<OkObjectResult>(actionResult.Result);
            var returnValue = Assert.IsType<OperationRequestDto>(okResult.Value);

            Assert.Equal(operationRequest.Id.AsGuid(), returnValue.Id);

        }


        [Fact]
        public async Task DeleteAsync_WithInValidId_ReturnsUnSuccess()
        {
            // Arrange
            var operationRequest = new OperationRequest(
                    "2025-01-01",
                    "eletric",
                    "patientId",
                    "operationTypeId",
                    "doctorThatRequestedId",
                    "doctorThatWillPerformId"
            );

            Guid idInvalid = Guid.NewGuid();


            _operationRequestRepository.Setup(repo => repo.GetByIdAsync(operationRequest.Id))
                    .ReturnsAsync(operationRequest);

            _operationRequestRepository.Setup(pr => pr.Remove(It.IsAny<OperationRequest>()));

            _unitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);


            bool flag = false;
            while (flag == false)
            {
                if (idInvalid != operationRequest.Id.AsGuid())
                {
                    flag = true;
                }
                idInvalid = Guid.NewGuid();
            }



            // Act
            var result = await _operationRequestController.Delete(idInvalid);

            // Assert
            var actionResult = Assert.IsType<ActionResult<OperationRequestDto>>(result);
            var badResult = Assert.IsType<BadRequestObjectResult>(actionResult.Result);


        }

        [Fact]
        public async Task GetOperationRequestsWithFilters_FilterByMedicalRecordNumber_ReturnsRequests()
        {
            // Arrange
            var filters = new OperationRequestFilterDto { MedicalRecordNumber = "202410000001" };
            var patient = new Patient("Jane Doe", "1990-01-01", "female", "jane.doe@example.com", "+351 1234567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202410000001");
            var patient2 = new Patient("Jane Doe", "1990-01-01", "female", "jane.doe@example.com", "+351 1234567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202310000001");
            OperationRequest op1 = new OperationRequest("2025-01-01", "eletric", patient.Id.AsString(), "operationTypeId", "doctorId", "doctorThatWillPerformId");
            OperationRequest op2 = new OperationRequest("2025-01-01", "eletric", patient2.Id.AsString(), "operationTypeId", "doctorId", "doctorThatWillPerformId");
            _patientRepository.Setup(repo => repo.GetByMedicalRecordNumberAsync(filters.MedicalRecordNumber))
                .ReturnsAsync(patient);
            _operationRequestRepository.Setup(repo => repo.GetOperationRequestsByPatientId(patient.Id.AsString()))
                .ReturnsAsync(new List<OperationRequest> { op1 });

            // Act
            var result = await _operationRequestController.GetOperationRequests(filters);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var returnValue = Assert.IsType<List<OperationRequestDto>>(okResult.Value);
            Assert.Single(returnValue); // Verifique o número de elementos na lista
        }

        [Fact]
        public async Task GetOperationRequestsWithFilters_FilterByPatientName_ReturnsRequests()
        {
            // Arrange
            var filters = new OperationRequestFilterDto { PatientName = "Jane Doe" };
            var patient = new Patient("Jane Doe", "1990-01-01", "female", "jane.doe@example.com", "+351 1234567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202410000001");
            var patient2 = new Patient("Jane Doe2", "1990-01-01", "female", "jane.doe@example.com", "+351 1234567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202310000001");
            OperationRequest op1 = new OperationRequest("2025-01-01", "eletric", patient.Id.AsString(), "operationTypeId", "doctorId", "doctorThatWillPerformId");
            OperationRequest op2 = new OperationRequest("2025-01-01", "eletric", patient2.Id.AsString(), "operationTypeId", "doctorId", "doctorThatWillPerformId");


            _patientRepository.Setup(repo => repo.GetByNameAsync(filters.PatientName))
                .ReturnsAsync(new List<Patient> { patient });
            _operationRequestRepository.Setup(repo => repo.GetOperationRequestsByPatientId(patient.Id.AsString()))
                .ReturnsAsync(new List<OperationRequest> { op1 });

            // Act
            var result = await _operationRequestController.GetOperationRequests(filters);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var returnValue = Assert.IsType<List<OperationRequestDto>>(okResult.Value);
            Assert.Equal(1, returnValue.Count); // Verifique o número de elementos na lista


        }

        [Fact]
        public async Task GetOperationRequestsWithFilters_FilterByDateRange_ReturnsRequests()
        {
            // Arrange
            var filters = new OperationRequestFilterDto
            {
                StartDate = DateTime.Parse("2025-01-01"),
                EndDate = DateTime.Parse("2025-12-31")
            };
            var operationRequests = new List<OperationRequest>
            {
                new OperationRequest("2025-01-01", "eletric", "patientId", "operationTypeId", "doctorId", "doctorThatWillPerformId"),
                new OperationRequest("2025-06-01", "eletric", "patientId", "operationTypeId", "doctorId", "doctorThatWillPerformId")
            };

            _operationRequestRepository.Setup(repo => repo.GetOperationRequestsByDoctorIdRequested(It.IsAny<string>()))
                .ReturnsAsync(operationRequests);

            // Act
            var result = await _operationRequestController.GetOperationRequests(filters);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var returnValue = Assert.IsType<List<OperationRequestDto>>(okResult.Value);
            Assert.Equal(2, returnValue.Count); // Verifique o número de elementos na lista

        }


        [Fact]
        public async Task GetOperationRequestsWithFilters_FilterByDateRange_ReturnsNotFound()
        {
            // Arrange
            var filters = new OperationRequestFilterDto
            {
                StartDate = DateTime.Parse("2026-01-01"),
                EndDate = DateTime.Parse("2029-12-31")
            };
            var operationRequests = new List<OperationRequest>
            {
                new OperationRequest("2025-01-01", "eletric", "patientId", "operationTypeId", "doctorId", "doctorThatWillPerformId"),
                new OperationRequest("2025-06-01", "eletric", "patientId", "operationTypeId", "doctorId", "doctorThatWillPerformId")
            };

            _operationRequestRepository.Setup(repo => repo.GetOperationRequestsByDoctorIdRequested(It.IsAny<string>()))
                .ReturnsAsync(operationRequests);


            // Act
            var result = await _operationRequestController.GetOperationRequests(filters);

            // Assert
            var actionResult = Assert.IsType<NotFoundResult>(result);
        }



    }
}






