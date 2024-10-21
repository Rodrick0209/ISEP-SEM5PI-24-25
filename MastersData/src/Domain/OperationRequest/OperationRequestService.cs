using System.Threading.Tasks;
using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.OperationRequestLoggers;
using DDDSample1.Domain.Utils;



namespace DDDSample1.Domain.OperationRequest
{
    public class OperationRequestService
    {
    private readonly IUnitOfWork _unitOfWork;

    private readonly IOperationRequestRepository _repo;

    
    // private readonly IAppointmentRepository _appointmentRepository;

    private readonly IPatientRepository _patientRepository;

    private readonly IOperationTypeRepository  _operationTypeRepository;

    private readonly IStaffRepository _staffRepository;
    
    private readonly IOperationRequestLoggerRepository _operationRequestLoggerRepository;

    public OperationRequestService(IUnitOfWork unitOfWork, IOperationRequestRepository repo,IOperationTypeRepository operationTypeRepository, IStaffRepository staffRepository, IPatientRepository patientRepository, IOperationRequestLoggerRepository operationRequestLoggerRepository)
    {
        this._unitOfWork = unitOfWork;
        this._repo = repo;
        this._operationTypeRepository = operationTypeRepository;
        this._staffRepository = staffRepository;
        this._patientRepository = patientRepository;
        this._operationRequestLoggerRepository = operationRequestLoggerRepository;
       // this._appointmentRepository = appointmentRepository;
    }


   

    public async Task<OperationRequest> AddAsync(OperationRequest operationRequest)
    {
         await checkOperationTypeIdAsync(operationRequest.operationTypeId);
         
        // await checkDoctorIdAsync(operationRequest.doctorId);
        // await checkPatientAsync(operationRequest.patientId);
        


        await this._repo.AddAsync(operationRequest);

        // falta adicionar o operation request ao medical history



        await this._unitOfWork.CommitAsync();

        return operationRequest;

    }

    public async Task<OperationRequest> UpdateAsync(ChangeOperationRequestDto dto, string doctorThatWantsToUpdateEmail)
    {
        var op = await this._repo.GetByIdAsync(new OperationRequestId(dto.Id));
        Email email = new Email(doctorThatWantsToUpdateEmail);
        var doctorThatRequestedId = email.getFirstPartOfEmail();
        
        if (op == null)
        {
            throw new BusinessRuleValidationException("Operation Request not found");
        }

        if (!op.doctorThatRequestedId.Equals(doctorThatRequestedId))
        {
            throw new BusinessRuleValidationException("Doctor not allowed to update this operation request");
        }

        bool hasChanges = false;

        if (!string.IsNullOrWhiteSpace(dto.Priority) && !op.priority.priority.Equals(dto.Priority))
        {
            op.ChangePriority(dto.Priority);
            hasChanges = true;
        }

        if (!string.IsNullOrWhiteSpace(dto.DeadLineDate) && !op.deadLineDate.deadLineDate.Equals(dto.DeadLineDate))
        {
            op.ChangeDeadLineDate(dto.DeadLineDate);
            hasChanges = true;
        }

        if (hasChanges)
        {
            var objetoLogger = LogObjectCreate(op, LoggerTypeOfChange.Update);
            await this._operationRequestLoggerRepository.AddAsync(objetoLogger);
        }

        await this._unitOfWork.CommitAsync();
        return op;
    }


    private OperationRequestLogger LogObjectCreate(OperationRequest operationRequest, LoggerTypeOfChange typeOfChange)
    {
       return new OperationRequestLogger(operationRequest.deadLineDate.deadLineDate,operationRequest.priority.priority,operationRequest.operationTypeId,operationRequest.doctorThatWillPerformId ,operationRequest.doctorThatRequestedId,operationRequest.Id.AsString(), typeOfChange.ToString());
    }
    


    public async Task<OperationRequest> DeleteAsync(OperationRequestId id)
    {
        //var appointment = checkOperationRequestIsAppointementAsync
        // if (appointment == null)
        //     throw new BusinessRuleValidationException("Operation Request is an appointment");

        var op = await this._repo.GetByIdAsync(id);

        if (op == null)
            return null;

        this._repo.Remove(op);
        await this._unitOfWork.CommitAsync();

        return op;    

    }



    

    public async Task<OperationRequest> GetByIdAsync(OperationRequestId id)
    {
        
        var op = await this._repo.GetByIdAsync(id);
        if (op == null)
            return null;

        return op;

    }

    public async Task<List<OperationRequest>> GetOperationRequestsWithFilters(OperationRequestFilterDto filters, string doctorId)
    {
            return await this._repo.GetOperationRequestsWithFilters(filters, doctorId);
    }





    public async Task<OperationType> checkOperationTypeIdAsync(OperationTypeId operationTypeId)
    {

        var opType = await this._operationTypeRepository.GetByIdAsync(operationTypeId);
        if (opType == null)
        {
            throw new BusinessRuleValidationException("Operation Type not found");
        }

        return opType;
    }

    public async Task<OperationType> checkOperationTypeIdAsync(string operationTypeId)
    {
        try
        {
            var id = new OperationTypeId(operationTypeId);
            Console.WriteLine("ID ->: " + id);
            var opType = await this._operationTypeRepository.GetByIdAsync(id);

            if (opType == null)
            {
                throw new BusinessRuleValidationException("Operation Type not found");
            }

            return opType;
        }
        catch (Exception e)
        {
            throw new BusinessRuleValidationException ("Operation Type Not Found");
        }
    }



 
    
    public async Task<Staff> checkDoctorIdAsync(StaffId doctorId)
    {

        var staff = await this._staffRepository.GetByIdAsync(doctorId);
        
        if(staff == null || !staff.Category.Equals("Doctor"))
        {
            throw new BusinessRuleValidationException("Doctor invalid");
        }

        return staff;


    }

    public async Task<List<OperationRequest>> GetAllAsync()
    {    
        return await this._repo.GetAllAsync();
    }

    public async Task<Patient> checkPatientAsync(PatientId id)
    {
        var patient = await this._patientRepository.GetByIdAsync(id);
        if(patient == null)
            throw new BusinessRuleValidationException("Patient not found");
        return patient;
    }



/*
    public async Task<Appointment> checkOperationRequestIsAppointementAsync(OperationRequestId id)
    {
        var appointment = await this._appointmentRepository.GetByOperationRequestId(id);
        if(appointment != null)
           return appointment;           
    }




    */




}
}