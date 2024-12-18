import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { AllergyCatalogItem } from '../models/allergyCatalog';
import { AllergyCatalogMapper } from '../mappers/allergyCatalogMapper';
import { code } from 'three/webgpu';

@Injectable({
  providedIn: 'root'
})
export class AllergyCatalogService {
  private baseUrl = '/api2/allergiesCatalog';
  private getAllergiesUrl = '/getAll';
  private getAllergyUrl = '/get';
  private createUrl = '/create';
  private updateUrl = '/update';

  //private baseUrl = 'http://localhost:4000/api2/allergiesCatalog';

  
  constructor(private http: HttpClient) {}

  getAllergiesFromCatalog(): Observable<AllergyCatalogItem[]> {
    const url = `${this.baseUrl}${this.getAllergiesUrl}`;
    return this.http.get<AllergyCatalogItem[]>(url).pipe(
      map((data: AllergyCatalogItem[]) => AllergyCatalogMapper.mapToAllergyCatalogItems(data))
    );
  }

  createAllergyCatalogItem(code: string, designation: string, description: string): Observable<AllergyCatalogItem> {
    const url = `${this.baseUrl}${this.createUrl}`;
    const body: any = {};
    body.code = code;
    body.designation = designation;
    if(description) {
      body.description = description;
    }
    return this.http.post<AllergyCatalogItem>(url, body).pipe(
      map((data: AllergyCatalogItem) => AllergyCatalogMapper.mapToAllergyCatalogItem(data))
    );
  }

  getAllergyCatalogItem(code: string): Observable<AllergyCatalogItem> {
    const url = `${this.baseUrl}${this.getAllergyUrl}/${code}`;
    return this.http.get<AllergyCatalogItem>(url).pipe(
      map((data: AllergyCatalogItem) => AllergyCatalogMapper.mapToAllergyCatalogItem(data))
    );
  }

  updateAllergyCatalogItem(code: string, designation: string, description: string): Observable<AllergyCatalogItem> {
    const url = `${this.baseUrl}${this.updateUrl}/${code}`;
    const body: any = {};
    if(designation) {
      body.designation = designation;
    }
    if(description) {
      body.description = description;
    }
    return this.http.put<AllergyCatalogItem>(url, body).pipe(
      map((data: AllergyCatalogItem) => AllergyCatalogMapper.mapToAllergyCatalogItem(data))
    );
  }
 
}