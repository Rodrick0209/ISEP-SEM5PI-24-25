using DDDSample1.Domain.User;
using DDDSample1.Infrastructure.Shared;
using System.Threading.Tasks;
using System.Collections.Generic;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.Users{

    public class UserRepository : BaseRepository<User, UserId> , IUserRepository
    {
       private readonly DDDSample1DbContext context;
        public UserRepository(DDDSample1DbContext context) : base(context.Users)
        {
            this.context = context;
        }

        public async Task<bool> CheckEmail(string email)
        {
            return await this.context.Users.AnyAsync(u => u.email.email == email);
        }
        
        public async Task<User> GetByEmailAsync(string email)
        {
            return await this.context.Users.FirstOrDefaultAsync(u => u.email.email == email);
        }
    }

}