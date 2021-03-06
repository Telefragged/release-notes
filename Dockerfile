FROM mcr.microsoft.com/dotnet/core/sdk:3.1-alpine AS builder
WORKDIR /src

COPY paket.dependencies paket.lock .config ./
RUN dotnet tool restore &&\
    dotnet paket restore

COPY . ./
RUN dotnet publish -c Release -o publish

FROM mcr.microsoft.com/dotnet/core/runtime:3.1-alpine
COPY --from=builder /src/publish /app
ENTRYPOINT ["dotnet", "/app/ReleaseBot.dll"]
