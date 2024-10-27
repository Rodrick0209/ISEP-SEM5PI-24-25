using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.OperationRequestLoggers;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.StaffMembers;
using Moq;
using Xunit;

public class OperationRequestServiceTests
{
    private readonly Mock<IUnitOfWork> _unitOfWorkMock;
    private readonly Mock<IOperationRequestRepository> _operationRequestRepositoryMock;
    private readonly Mock<IOperationTypeRepository> _operationTypeRepositoryMock;
    private readonly Mock<IStaffRepository> _staffRepositoryMock;
    private readonly Mock<IPatientRepository> _patientRepositoryMock;
    private readonly Mock<IOperationRequestLoggerRepository> _operationRequestLoggerRepositoryMock;
    private readonly OperationRequestService _service;

    public OperationRequestServiceTests()
    {
        _unitOfWorkMock = new Mock<IUnitOfWork>();
        _operationRequestRepositoryMock = new Mock<IOperationRequestRepository>();
        _operationTypeRepositoryMock = new Mock<IOperationTypeRepository>();
        _staffRepositoryMock = new Mock<IStaffRepository>();
        _patientRepositoryMock = new Mock<IPatientRepository>();
        _operationRequestLoggerRepositoryMock = new Mock<IOperationRequestLoggerRepository>();

        _service = new OperationRequestService(
            _unitOfWorkMock.Object,
            _operationRequestRepositoryMock.Object,
            _operationTypeRepositoryMock.Object,
            _staffRepositoryMock.Object,
            _patientRepositoryMock.Object,
            _operationRequestLoggerRepositoryMock.Object
        );
    }


    [Fact]
    public async Task AddAsync_ValidOperationRequest_ReturnsOperationRequest()
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

        var availabilitySlot1 = new AvailabilitySlot("test");


        // Configurar mocks para retornar entidades com esses IDs fixos
        _operationTypeRepositoryMock.Setup(repo => repo.GetByIdAsync(new OperationTypeId(operationTypeId)))
            .ReturnsAsync(new OperationType("New Operation Type", true, phase1, phase2, phase3, specialization1.Id));

        _patientRepositoryMock.Setup(repo => repo.GetByIdAsync(new PatientId(patientId)))
            .ReturnsAsync(new Patient("Jane Doe", "1990-01-01", "female", "john.doe2@example.com", "+351 1232567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202410000001"));

        _staffRepositoryMock.Setup(repo => repo.GetByIdAsync(new StaffId(staffId)))
            .ReturnsAsync(new Staff(new StaffId("D202512345"), "Miguel", "12345", specialization1.Id.AsString(), availabilitySlot1.Id.AsString(), "mamarNoC@gmail.com", "+351123456789", "Doctor","true"));

        _unitOfWorkMock.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

        // Criar DTO com IDs válidos para o teste
        var dto = new OperationRequestDto
        (
            "2025-01-01",
            "eletric",
            patientId.ToString(),
            operationTypeId.ToString(),
            staffId.ToString()
        );

        OperationRequest operationRequest = new OperationRequest("2025-01-01", "eletric", patientId.ToString(), operationTypeId.ToString(), staffId.ToString(), staffId.ToString());


        // Act
        var result = await _service.AddAsync(operationRequest);



        // Assert
        Assert.NotNull(result);
        _operationRequestRepositoryMock.Verify(repo => repo.AddAsync(It.IsAny<DDDSample1.Domain.OperationRequest.OperationRequest>()), Times.Once);
        _unitOfWorkMock.Verify(uow => uow.CommitAsync(), Times.Once);
    }


    

    [Fact]
    public async Task DeleteAsync_ValidId_ReturnsOperationRequest()
    {
        // Arrange
        var operationRequestId = new OperationRequestId(Guid.NewGuid());
        var operationRequest = new OperationRequest(
            "2025-01-01",
            "eletric",
            "patientId",
            "operationTypeId",
            "doctorThatRequestedId",
            "doctorThatWillPerformId"
        );

        _operationRequestRepositoryMock.Setup(repo => repo.GetByIdAsync(operationRequestId))
            .ReturnsAsync(operationRequest);


        _unitOfWorkMock.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);


        // Act
        var result = await _service.DeleteAsync(operationRequestId);

        // Assert
        Assert.NotNull(result);
        _operationRequestRepositoryMock.Verify(repo => repo.Remove(It.IsAny<DDDSample1.Domain.OperationRequest.OperationRequest>()), Times.Once);
        _unitOfWorkMock.Verify(uow => uow.CommitAsync(), Times.Once);
    }

    [Fact]
    public async Task UpdateAsync_ValidDto_ReturnsUpdatedOperationRequest()
    {
        // Arrange
        
        
        
        var operationRequestId = new OperationRequestId(Guid.NewGuid());
        var operationRequest = new OperationRequest(
            "2025-01-01",
            "eletric",
            "patientId",
            "operationTypeId",
            "doctorThatRequestedId",
            "doctorThatWillPerformId"
        );

        var changeOperationRequestDto = new ChangeOperationRequestDto(
            operationRequestId.AsGuid(),
            "2029-01-01",
            "urgency"
        );

        _operationRequestRepositoryMock.Setup(repo => repo.GetByIdAsync(operationRequestId))
            .ReturnsAsync(operationRequest);


        _unitOfWorkMock.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);


        // Act
        var result = await _service.UpdateAsync(changeOperationRequestDto, "doctorThatRequestedId@example.com");

        // Assert
        Assert.NotNull(result);
        Assert.Equal(changeOperationRequestDto.DeadLineDate, result.deadLineDate.deadLineDate);
        Assert.Equal(changeOperationRequestDto.Priority, result.priority.priority);
        _operationRequestLoggerRepositoryMock.Verify(repo => repo.AddAsync(It.IsAny<DDDSample1.Domain.OperationRequestLoggers.OperationRequestLogger>()), Times.Once);
        _unitOfWorkMock.Verify(uow => uow.CommitAsync(), Times.Once);

    }



    [Fact]
    public async Task GetOperationRequestsWithFilters_FilterByMedicalRecordNumber_ReturnsRequests()
    {
        // Arrange
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

        var availabilitySlot1 = new AvailabilitySlot("test");
        
        var filters = new OperationRequestFilterDto { MedicalRecordNumber = "202410000001" };
        var patient = new Patient("Jane Doe", "1990-01-01", "female", "jane.doe@example.com", "+351 1234567890", "123 Main St", "12345", "Anytown", "Anycountry", "Jane Doe", "jane.doe@example.com", "+351 0987654321", "202410000001");
        var operationRequests = new List<OperationRequest>
        {
            new OperationRequest("2025-01-01", "eletric", patient.Id.AsString(), "operationTypeId", "doctorId", "doctorThatWillPerformId")
        };

        _patientRepositoryMock.Setup(repo => repo.GetByMedicalRecordNumberAsync(filters.MedicalRecordNumber))
            .ReturnsAsync(patient);
        _operationRequestRepositoryMock.Setup(repo => repo.GetOperationRequestsByPatientId(patient.Id.AsString()))
            .ReturnsAsync(operationRequests);
        _staffRepositoryMock.Setup(repo => repo.GetByIdAsync(It.IsAny<StaffId>()))
            .ReturnsAsync(new Staff(new StaffId("D202512345"), "Miguel", "12345", specialization1.Id.AsString(), availabilitySlot1.Id.AsString(), "mamarNoC@gmail.com", "+351123456789", "Doctor","true"));

        _unitOfWorkMock.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

        // Act
        var result = await _service.GetOperationRequestsWithFilters(filters, "doctor@example.com");

        // Assert
        Assert.Single(result);
        Assert.Equal(patient.Id.AsString(), result.First().PatientId);
    }
}