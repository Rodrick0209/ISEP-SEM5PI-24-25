using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDSample1.Domain.User
{

    public interface IUserRepository : IRepository<User, UserId>
    {
    
    }






}