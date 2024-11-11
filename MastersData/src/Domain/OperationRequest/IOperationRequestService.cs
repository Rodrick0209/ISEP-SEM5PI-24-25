
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.OperationRequest;


namespace DDDSample1.Domain.OperationRequest
{

    public interface IOperationRequestService
    {
        Task<OperationRequest> AddAsync(OperationRequest operationRequest);
        Task<OperationRequest> UpdateAsync(ChangeOperationRequestDto dto, string doctorThatWantsToUpdateEmail);
        Task<OperationRequest> DeleteAsync(OperationRequestId id);
        Task<List<OperationRequestDto>> GetOperationRequestsWithFilters(OperationRequestFilterDto filters, string doctorIdEmail);
        Task<List<OperationRequest>> GetAllAsync();
        Task<List<OperationRequestDto>> GetAllForUiAsync(string emailDoctorQuerEditar);

        Task<OperationRequest> GetByIdAsync(OperationRequestId id);




    }


}