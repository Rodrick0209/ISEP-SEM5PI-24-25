// remove by JRT : import jwt from 'express-jwt';
var { expressjwt: jwt } = require("express-jwt");
import { Console } from 'console';
import config from '../../../config';
import Logger from '../../loaders/logger';


/**
 * We are assuming that the JWT will come in a header with the form
 *
 * Authorization: Bearer ${JWT}
 *
 * But it could come in a query parameter with the name that you want like
 * GET https://my-bulletproof-api.com/stats?apiKey=${JWT}
 * Luckily this API follow _common sense_ ergo a _good design_ and don't allow that ugly stuff
 */
const getTokenFromHeader = req => {
  if (
    (req.headers.authorization && req.headers.authorization.split(' ')[0] === 'Token') ||
    (req.headers.authorization && req.headers.authorization.split(' ')[0] === 'Bearer')
  ) {
    const token = req.headers.authorization.split(' ')[1];
    Logger.info(`Token found in header: ${token}`);
    return token;
  }
  Logger.info("No token found in header");
  return null;
};


const isAuth = jwt({
  secret: config.jwtSecret, // The _secret_ to sign the JWTs
  userProperty: 'token', // Use req.token to store the JWT
  getToken: getTokenFromHeader, // How to extract the JWT from the request
  algorithms: ["HS256"],  // Added by JRT
});

export default isAuth;
