using DDDSample1.Infrastructure.Shared;
using DDDSample1.Domain.OperationRequest;
using System.Threading.Tasks;
using System.Collections.Generic;
using Microsoft.EntityFrameworkCore;
using System.Linq;



namespace DDDSample1.Infrastructure.OperationRequests
{

    public class OperationRequestRepository : BaseRepository<OperationRequest, OperationRequestId>, IOperationRequestRepository
    {

        private readonly DDDSample1DbContext context;

        public OperationRequestRepository(DDDSample1DbContext context):base(context.OperationRequests)
        {
            this.context = context;
        }


        public async Task<List<OperationRequest>> GetOperationRequestsWithFilters(OperationRequestFilterDto filters, string doctorId)
        {
            var query = context.OperationRequests
                    .Join(context.Patients,
                        or => or.patientId,
                        p => p.Id,
                        (or, p) => new { OperationRequest = or, PatientName = p.FullName.fullName })
                    .Where(op => op.OperationRequest.doctorId.Equals(doctorId) ||
                                (filters.PatientId != null && op.OperationRequest.patientId.Equals(filters.PatientId)));

                if (!string.IsNullOrEmpty(filters.PatientName))
                {
                    query = query.Where(op => op.PatientName.Contains(filters.PatientName));
                }

                if (!string.IsNullOrEmpty(filters.OperationType))
                {
                    query = query.Where(op => op.OperationRequest.operationTypeId.Equals(filters.OperationType));
                }

                if (!string.IsNullOrEmpty(filters.Priority))
                {
                    query = query.Where(op => op.OperationRequest.priority.Equals(filters.Priority));
                }

                return await query.Select(op => op.OperationRequest).ToListAsync();
        }
    }

}