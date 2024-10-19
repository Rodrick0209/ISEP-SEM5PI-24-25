using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Infrastructure.Families;
using DDDSample1.Domain.OperationType;



namespace DDDSample1.Domain.StaffMembers
{
    public class StaffService
    {
    private readonly IUnitOfWork _unitOfWork;

    private readonly IStaffRepository _staffRepository;

    

        public StaffService(IUnitOfWork unitOfWork, IStaffRepository staffRepository)
        {
            _unitOfWork = unitOfWork;
            _staffRepository = staffRepository;
            
        }

// */
    


//     public async Task<OperationRequest> AddAsync(OperationRequest operationRequest)
//     {
//         // await checkOperationTypeIdAsync(operationRequest.OperationTypeId);
//         // await checkDoctorIdAsync(operationRequest.DoctorId);
//         // falta adicionar o operation request ao medical history


//         await this._repo.AddAsync(operationRequest);

//         await this._unitOfWork.CommitAsync();

//         return operationRequest;

//     }

//     public async Task<OperationRequest> UpdateAsync(OperationRequestDto dto)
//     {
//       //  await checkOperationTypeIdAsync(dto.OperationTypeId);
//       //  await checkDoctorIdAsync(dto.DoctorId);
    
//         var op = await this._repo.GetByIdAsync(new OperationRequestId(dto.Id));
//         if (op == null)
//             return null;

//             op.ChangeOperationTypeId(dto.OperationTypeId);
//             op.ChangeDoctorId(dto.DoctorId);
//             op.ChangePriority(dto.Priority);
//             op.ChangeDeadLineDate(dto.DeadLineDate);
//             op.ChangePatientId(dto.PatientId);

//             await this._unitOfWork.CommitAsync();

//             return op;

//     }


//     public async Task<OperationRequest> DeleteAsync(OperationRequestId id)
//     {
//         //var appointment = checkOperationRequestIsAppointementAsync
//         // if (appointment == null)
//         //     throw new BusinessRuleValidationException("Operation Request is an appointment");

//         var op = await this._repo.GetByIdAsync(id);

//         if (op == null)
//             return null;

//         this._repo.Remove(op);
//         await this._unitOfWork.CommitAsync();

//         return op;    

//     }



    

//     public async Task<OperationRequest> GetByIdAsync(OperationRequestId id)
//     {
        
//         var op = await this._repo.GetByIdAsync(id);
//         if (op == null)
//             return null;

//         return op;

//     }

//     public async Task<List<OperationRequest>> GetOperationRequestsWithFilters(OperationRequestFilterDto filters, string doctorId)
//     {
//             return await this._repo.GetOperationRequestsWithFilters(filters, doctorId);
//     }





//  /*   public async Task checkOperationTypeIdAsync(OperationTypeId operationTypeId)
//     {

//         var opType = await this._operationTypeRepository.GetByIdAsync(operationTypeId);
//         if (opType == null)
//         {
//             throw new BusinessRuleValidationException("Operation Type not found");
//         }
//     }
// */
 
 
//     /*
//     public async Task checkDoctorIdAsync(StaffId doctorId)
//     {

//         var doctor = await this._staffRepository.GetDoctorById(doctorId);
//         if(doctor == null)
//         {
//             throw new BusinessRuleValidationException("Doctor invalid");
//         }


//     }

//     public async Task checkOperationRequestIsAppointementAsync(OperationRequestId id)
//     {
//         var appointment = await this._appointmentRepository.GetByOperationRequestId(id);
//         if(appointment != null)
//            return appointment;           
//     }




//     */


// }
}
}