using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Infrastructure.Shared;
using DDDSample1.Domain.StaffMembers;
using Microsoft.AspNetCore.Builder.Extensions;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.StaffMembers
{
    public class StaffRepository : BaseRepository<Staff, StaffId>, IStaffRepository
    {
        private readonly DDDSample1DbContext context;

        public StaffRepository(DDDSample1DbContext context) : base(context.StaffMembers)
        {
            this.context = context;
        }

        public async Task<Staff> GetByEmailAsync(string email)
        {
            return await this.context.StaffMembers.FirstOrDefaultAsync(e => e.Email.email == email);

        }

        public async Task<Staff> GetByPhoneNumberAsync(string phoneNumber)
        {
            return await this.context.StaffMembers.FirstOrDefaultAsync(p => p.PhoneNumber.phoneNumber == phoneNumber);
        }
        public async Task DeleteAsync(StaffId id)
        {
            var staff = await this.context.StaffMembers.FindAsync(id);
            if (staff != null)
            {
                this.context.StaffMembers.Remove(staff);
                await this.context.SaveChangesAsync();
            }
        }

        public async Task<Staff> GetByLicenseNumberAsync(string licenseNumber)
        {
            return await this.context.StaffMembers.FirstOrDefaultAsync(ln => ln.LicenseNumber.licenseNumber == licenseNumber);
        }

        public async Task<Staff> GetByNameAsync(string name)
        {
            return await this.context.StaffMembers.FirstOrDefaultAsync(n => n.FullName.fullName == name);
        }

        public async Task<Staff> GetByIdsAsync(string id)
        {
            return await this.context.StaffMembers.FirstOrDefaultAsync(s => s.Id.AsString() == id);


            ;
        }





        public async Task<List<Staff>> GetByFiltersAsync(string name, string licenseNumber, string phoneNumber, string email, string specialization)
        {

            

            var query = this.context.StaffMembers.AsQueryable();

            // Filtro por nome, se fornecido
            if (!string.IsNullOrWhiteSpace(name))
            {
                query = query.Where(p => p.FullName.fullName.ToLower().Contains(name.ToLower()));
            }

            // Filtro por número de licença, se fornecido
            if (!string.IsNullOrWhiteSpace(licenseNumber))
            {
                query = query.Where(p => p.LicenseNumber.licenseNumber.ToLower().Contains(licenseNumber.ToLower()));
            }

            // Filtro por número de telefone, se fornecido
            if (!string.IsNullOrWhiteSpace(phoneNumber))
            {
                query = query.Where(p => p.PhoneNumber.phoneNumber.ToLower().Contains(phoneNumber.ToLower()));
            }

            // Filtro por e-mail, se fornecido
            if (!string.IsNullOrWhiteSpace(email))
            {
                query = query.Where(p => p.Email.email.ToLower().Contains(email.ToLower()));
            }

            if (!string.IsNullOrEmpty(specialization))
            {
                query = query.Where(p => p.SpecializationId.Value.ToLower().Contains(specialization.ToLower()));
            }


            // Executa a consulta e retorna os resultados
            return await query.ToListAsync();
        }


    }
}
