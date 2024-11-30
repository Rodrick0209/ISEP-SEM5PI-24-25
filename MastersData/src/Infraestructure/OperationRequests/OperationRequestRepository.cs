using DDDSample1.Infrastructure.Shared;
using DDDSample1.Domain.OperationRequest;
using System.Threading.Tasks;
using System.Collections.Generic;
using Microsoft.EntityFrameworkCore;
using System.Linq;
using System.Runtime.CompilerServices;
using System;




namespace DDDSample1.Infrastructure.OperationRequests
{

    public class OperationRequestRepository : BaseRepository<OperationRequest, OperationRequestId>, IOperationRequestRepository
    {

        private readonly DDDSample1DbContext context;

        public OperationRequestRepository(DDDSample1DbContext context) : base(context.OperationRequests)
        {
            this.context = context;
        }

        public Task<List<OperationRequest>> OperationRequestsAsQueyable()
        {
            return this.context.OperationRequests.AsQueryable().ToListAsync();
        }



        public async Task<List<OperationRequest>> GetAllAsync()
        {
            return await this.context.OperationRequests
                .ToListAsync();
        }

        public async Task<OperationRequest> GetByIdAsync(OperationRequestId id)
        {
            return await this.context.OperationRequests
                .Include(o => o.staffAssignedSurgery)
                .FirstOrDefaultAsync(or => or.Id == id);
        }

        public async Task<List<OperationRequest>> GetOperationRequestsByDoctorIdRequested(string doctorId)
        {
            return await this.context.OperationRequests
                .Where(or => or.doctorThatRequestedId == doctorId)
                .ToListAsync();
        }

        public async Task<List<OperationRequest>> GetOperationRequestsByPatientId(string patientId)
        {
            return await this.context.OperationRequests
                .Where(or => or.patientId == patientId)
                .ToListAsync();
        }


        public Task<List<OperationRequest>> GetOperationRequestsByOperationTypeFromAList(string operationTypeId, List<OperationRequest> operationRequests)
        {
            // Filtra as OperationRequests e converte para uma lista
            var filteredRequests = operationRequests
                .Where(or => or.operationTypeId == operationTypeId) // Certifique-se de comparar corretamente
                .ToList(); // Use ToList() para converter para lista em memória

            return Task.FromResult(filteredRequests); // Retorna a lista filtrada como uma Task
        }


        public async Task<List<OperationRequest>> GetAllWaitingAsync()
        {
            return await this.context.OperationRequests
                .Where(or => or.status == OperationRequestStatus.Waiting)
                .ToListAsync();
        }



    }

}