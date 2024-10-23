using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.JsonPatch.Operations;
using System.Collections.Generic;
using System.Threading.Tasks;


namespace DDDSample1.Domain.OperationRequest
{

    public interface IOperationRequestRepository:IRepository<OperationRequest,OperationRequestId>
    {
            Task<List<OperationRequest>> OperationRequestsAsQueyable();
            Task<List<OperationRequest>> GetOperationRequestsByDoctorIdRequested(string doctorId);
            Task<List<OperationRequest>> GetOperationRequestsByPatientId(string patientId);

            Task<List<OperationRequest>> GetOperationRequestsByOperationTypeFromAList(string operationTypeId, List<OperationRequest> operationRequests);

    }


}